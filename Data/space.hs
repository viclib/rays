{-# LANGUAGE TypeFamilies, Rank2Types, FlexibleInstances, LambdaCase, TupleSections, DeriveGeneric #-}

module Data.Space where

import Prelude hiding (foldr1,sum,foldr,mapM_)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Foldable
import Data.Traversable
import Linear hiding (trace)
import Control.Monad.Primitive
import Control.Monad.Free
import Linear.Epsilon
import Control.Monad.ST
import Data.STRef
import Control.Monad hiding (forM_,mapM_)
import Text.PrettyPrint.Leijen
import Debug.Trace
import Data.Maybe
import Control.DeepSeq
import Control.DeepSeq.Generics
import GHC.Generics

data Element f e  = Element { getKey :: f, getElement :: e } deriving (Show, Generic)
data Space f e    = Space (V.Vector (Space f e)) | Container (V.Vector (Element f e)) deriving (Show, Generic)
data MSpace s f e = MSpace (MV.MVector s (MSpace s f e)) | MContainer Int (MV.MVector s (Element f e))
instance (NFData f, NFData e) => NFData (Element f e) where rnf = genericRnf
instance (NFData f, NFData e) => NFData (Space f e) where rnf = genericRnf

class PartitionKey a where
    branches :: a -> Int
    drive    :: a -> [Int]

str :: (Show e) => Space (f a) e -> Doc
str (Space spaces) = nest 1 (text "(" <> (foldl' (<$>) empty (V.toList (V.map str spaces))) <> text ")")
str (Container elements) = text "[" <> fillSep (V.toList (V.map (text . show . getElement) elements)) <> text "]"

getContainerAndDepth :: (PartitionKey (f a), Integral c) => Space (f a) b -> f a -> (V.Vector (Element (f a) b), c)
getContainerAndDepth (Container elements) _ = (elements,0)
getContainerAndDepth spaces key             = go spaces (drive key) 0 where 
    go (Space spaces) (i:is) depth  = go (spaces V.! i) is (depth+1)
    go (Container elements) _ depth = (elements,depth)

getContainer :: (PartitionKey (f a)) => Space (f a) b -> f a -> V.Vector (Element (f a) b)
getContainer spaces key = fst $ getContainerAndDepth spaces key

fromList :: (PartitionKey (f a)) => Int -> [(f a, e)] -> Space (f a) e
fromList cap keyVals = runST $ do
    space <- newSpace (branches . fst . head $ keyVals) cap
    forM_ keyVals $ \ (key,val) ->
        insert space key val
    unsafeFreeze space

newSpace :: (PrimMonad m) => Int -> Int -> m (MSpace (PrimState m) (f a) e)
newSpace branches containerCapacity = do
    spaces <- MV.new branches
    forM_ [0..branches] $ \i -> do
        container <- MV.new containerCapacity
        MV.unsafeWrite spaces i (MContainer 0 container)
    return $ MSpace spaces

insert :: (PrimMonad m, PartitionKey (f a)) => MSpace (PrimState m) (f a) b -> f a -> b -> m ()
insert space key value = go space key value (drive key) 0 where
    go :: (PrimMonad m, PartitionKey (f a)) => MSpace (PrimState m) (f a) b -> f a -> b -> [Int] -> Int -> m ()
    go space@(MSpace spaces) key value (index:nextIndexes) depth =
        MV.unsafeRead spaces index >>= \case
            nextSpace@(MSpace nextSpaces) -> go nextSpace key value nextIndexes (depth+1)
            MContainer count elements -> do
                let append = do
                    MV.unsafeWrite elements count (Element key value)
                    MV.unsafeWrite spaces index (MContainer (count+1) elements) 
                let split = do
                    newSpace (branches key) (MV.length elements) >>= MV.unsafeWrite spaces index
                    forM_ [0..count-1] $ \ i -> do
                        (Element key element) <- MV.unsafeRead elements i 
                        go space key element (drop depth (drive key)) depth
                    go space key value (index:nextIndexes) depth
                if count >= MV.length elements 
                    then split 
                    else append 

applyFreeze :: PrimMonad m => (forall a. MV.MVector (PrimState m) a -> m (V.Vector a)) -> MSpace (PrimState m) (f a) e -> m (Space (f a) e)
applyFreeze fn (MSpace spaces) = fn spaces >>= V.mapM (applyFreeze fn) >>= return . Space
applyFreeze fn (MContainer size elements) = fn (MV.slice 0 size elements) >>= return . Container

freeze, unsafeFreeze :: (PrimMonad m) => MSpace (PrimState m) (f a) e -> m (Space (f a) e)
freeze       = applyFreeze V.freeze
unsafeFreeze = applyFreeze V.unsafeFreeze
