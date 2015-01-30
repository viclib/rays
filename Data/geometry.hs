{-# LANGUAGE DeriveFunctor, MultiParamTypeClasses, TemplateHaskell, TypeFamilies, FlexibleInstances, RankNTypes, FlexibleContexts, TupleSections, DeriveGeneric #-}

module Data.Geometry where

import Linear hiding (trace)
import Data.Maybe
import Prelude hiding (foldr, foldr1, and, all, sum)
import Data.Foldable
import Data.Traversable
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Generic as GV
import Data.Vector.Unboxed.Deriving
import Control.Monad
import Data.Space
import Debug.Trace
import GHC.Generics
import Control.DeepSeq
import Control.DeepSeq.Generics

type Quad v a    = V4 (v a)
type Surface v a = a -> a -> v a
type Mesh v a    = [Quad v a]
data AABB v a    = AABB {aabbMin :: v a, aabbMax :: v a} deriving (Show, Generic)
data Ray v a     = Ray {rayPos :: v a, rayDir :: v a} deriving (Show, Generic)
data Plane v a   = Plane {planePos :: v a, planeNorm :: v a} deriving (Show, Generic)
data Sphere v a  = Sphere {spherePos :: v a, sphereRad :: a} deriving (Show, Generic)
data Pivot v a   = Pivot {pivotPos :: v a, pivotDir :: v a, pivotUp :: v a} deriving (Show, Generic)
instance (NFData (v a), NFData a) => NFData (AABB v a)   where rnf = genericRnf
instance (NFData (v a), NFData a) => NFData (Ray v a)    where rnf = genericRnf
instance (NFData (v a), NFData a) => NFData (Plane v a)  where rnf = genericRnf
instance (NFData (v a), NFData a) => NFData (Sphere v a) where rnf = genericRnf
instance (NFData (v a), NFData a) => NFData (Pivot v a)  where rnf = genericRnf

xAxis, yAxis, zAxis :: (Num a) => V3 a
xAxis = V3 1 0 0
yAxis = V3 0 1 0
zAxis = V3 0 0 1

projectOnPlane :: (Metric f, Num (f a), Num a) => Plane f a -> f a -> f a
projectOnPlane (Plane pPos pNorm) pos = pos - pNorm ^* dot (pos - pPos) pNorm

toPivotCoordinates :: (Num a) => Pivot V3 a -> V3 a -> V3 a
toPivotCoordinates (Pivot pPos pDir pUp) pos = V3 x y z where
    x = dot p (cross pUp pDir)
    y = dot p pUp
    z = dot (pos - pPos) pDir
    p = (pos - pPos) - pDir ^* z

projectTibia :: (Num a, Fractional a) => V3 a -> V3 a
projectTibia (V3 x y z) = (V3 x z y) * (V3 0.008 0.008 0.008)

surfaceToMesh :: (Enum a, Fractional a) => a -> a -> Surface f a -> Mesh f a
surfaceToMesh p q surface = [makeQuad u v | v <- [0..p-1], u <- [0..q-1]] where
    makeQuad u v = V4 
        (surface (u/q)     (v/p)) 
        (surface ((u+1)/q) (v/p)) 
        (surface ((u+1)/q) ((v+1)/p)) 
        (surface (u/q)     ((v+1)/p))

hitPlane :: (Num (f a), Metric f, Num a, Fractional a) => Ray f a -> Plane f a -> f a
hitPlane (Ray rPos rDir) (Plane pPos pNorm) = rPos + dot (pPos - rPos) pNorm / dot rDir pNorm *^ rDir

hitQuad :: (Ord a, Fractional a) => Ray V3 a -> Quad V3 a -> Maybe (V3 a)
hitQuad ray (quad@(V4 a b c d)) = if hitInsideQuad then Just hitPoint else Nothing where 
    hitInsideQuad = insideQuad hitPoint quad
    planeNormal   = cross (b - a) (d - a)
    hitPoint      = hitPlane ray (Plane a planeNormal)

crossSphere :: (GV.Vector v (f a), GV.Vector v a, Metric f, Ord a, Num (f a), Floating a) => Ray f a -> Sphere f a -> v a
crossSphere (Ray rPos rDir) (Sphere sPos sRad) = GV.fromList (if d > 0 then ts else []) where
    a  = quadrance rDir
    b  = dot (2 *^ rDir) (rPos - sPos)
    c  = quadrance (rPos - sPos) - sRad ** 2
    d  = b ** 2 - 4 * a * c
    ts = [(-b+sqrt d)/(2*a), (-b-sqrt d)/(2*a)]

hitSphere :: (Metric f, Ord a, Num (f a), Floating a) => Ray f a -> Sphere f a -> Maybe (f a)
hitSphere ray@(Ray rPos rDir) sphere = hit where
    hit  = if not (V.null hits) then Just (rPos + (GV.minimum hits *^ rDir)) else Nothing
    hits = V.filter (> 0) $ crossSphere ray sphere

hitSpheres :: (Metric f, Ord a, Num (f a), Floating a) => Ray f a -> [Sphere f a] -> Maybe (f a)
hitSpheres ray = foldr mplus mzero . map (hitSphere ray)

crossAABB :: (Foldable f, Metric f, Ord a, Num (f a), Fractional (f a), Floating a) => Ray f a -> AABB f a -> [a]
crossAABB (Ray rPos rDir) (AABB aabbMin aabbMax) = [tmin, tmax] where
    t1   = (aabbMin - rPos)/rDir
    t2   = (aabbMax - rPos)/rDir
    tmin = foldr1 max $ liftI2 min t1 t2
    tmax = foldr1 min $ liftI2 max t1 t2

hitAABB :: (Foldable f, Metric f, Ord a, Num (f a), Fractional (f a), Floating a) => Ray f a -> AABB f a -> Maybe (f a)
hitAABB ray@(Ray rPos rDir) aabb 
    | tmin < tmax = Just $ rPos + tmin *^ rDir 
    | otherwise   = Nothing 
    where [tmin,tmax] = crossAABB ray aabb

-- improve this, shouldn't just add 1e-4 but consider normal instead, or won't work for close angles
passAABB :: (Foldable f, Metric f, Ord a, Num (f a), Fractional (f a), Floating a) => Ray f a -> AABB f a -> f a
passAABB ray@(Ray rPos rDir) aabb = rPos + (tmax + 1e-4) *^ rDir where [_,tmax] = crossAABB ray aabb

insideAABB :: (Foldable f, Additive f, Ord a) => f a -> AABB f a -> Bool
insideAABB pos (AABB from to) = and (liftI2 (<=) from pos) && and (liftI2 (<=) pos to)

insideQuad :: (Metric f, Ord a, Num (f a), Num a) => f a -> V4 (f a) -> Bool
insideQuad pos (V4 a b c d) = all inside borders where
    borders      = [(a,b),(b,c),(c,d),(d,a)]
    inside (a,b) = dot (b - a) (pos - a) > 0

interpolateList :: (RealFrac s, Fractional a) => [a] -> s -> a
interpolateList vals pos = valA * (1 - pos') + valB * pos' where
    floatIndex = fromIntegral (length vals - 1) * pos
    index      = min (floor floatIndex) (length vals - 2)
    pos'       = realToFrac $ floatIndex - fromIntegral index
    valA       = vals !! index
    valB       = vals !! (index + 1)

-- HYPEROCTREE

type Hyperoctree  = Space
type Quadtree a   = Hyperoctree (V2 a)
type Octree a     = Hyperoctree (V3 a)

instance (Foldable f, Functor f, Traversable f, Num a, Ord a, Fractional a) => PartitionKey (f a) where
    branches = (^ 2) . floor . sum . fmap (const 1)
    drive vec = orthantIndex vec : drive (fmap driveCoordinate vec) where
        orthantIndex = floor . foldr' (\ x idx -> idx * 2 + (if x>0 then 1 else 0)) 0
        driveCoordinate x = fst $ driveRange ((,(-1,1)) x)

driveRange :: (Ord a, Num a, Fractional a) => (a, (a, a)) -> (a, (a, a))
driveRange (x,(a,b)) = if x >= 0 then (2*x-1, ((a+b)*0.5,b)) else (2*x+1, (a,(a+b)*0.5))

queryRay :: (Eq (f a), Show (f a), Metric f, Traversable f, Ord a, Fractional (f a), Floating a) => Space (f a) b -> Ray f a -> [b]
queryRay space ray = map getElement $ queryRayKeyVals space ray

queryRayKeyVals :: (Eq (f a), Show (f a), Metric f, Traversable f, Ord a, Fractional (f a), Floating a) => Space (f a) b -> Ray f a -> [Element (f a) b]
queryRayKeyVals space ray@(Ray rPos rDir) = case hitAABB ray spaceAABB of { Just hit -> walk hit; Nothing -> [] } where
    callNTimes n f x = iterate f x !! fromIntegral n
    spaceAABB = AABB (-(fmap (const 1) rPos)) (fmap (const 1) rPos)
    walk pos | not (insideAABB pos spaceAABB) = []
    walk pos | otherwise = foldr (:) (walk nextPos) (V.toList container) where
        (container,depth) = getContainerAndDepth space pos
        posRange          = fmap (callNTimes depth driveRange . (,(-1,1))) pos
        aabb              = AABB (fmap (fst . snd) posRange) (fmap (snd . snd) posRange)
        nextPos           = passAABB ray aabb

hitHyperoctree :: (Eq (f a), Show (f a), PartitionKey (f a), Traversable f, Metric f, Ord a, Num (f a), Floating a, Fractional (f a))
               => Ray f a -> Space (f a) (Sphere f a) -> Maybe (f a)
hitHyperoctree ray space = hitSpheres ray $ queryRay space ray
