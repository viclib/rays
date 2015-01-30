import Control.Applicative
import Linear hiding (trace)
import Data.Geometry
import Data.Space
import Criterion.Main
import Control.DeepSeq
import Control.Benchmark
import Control.Monad

main = do
    print "Building space:"
    print =<< (benchmark $ octree)
    print "Casting rays:"
    print =<< (benchmark $ castRays 256)

castRays :: Float -> [Maybe (V3 Float)]
castRays resolution = [hit x y | x <- range, y <- range] where
    hit x y = hitHyperoctree (Ray (V3 x (-1) y) (V3 0 1 0)) octree
    range   = [-0.9,-(0.9 - 1.8/resolution)..0.9]

spheres :: [(V3 Float, Sphere V3 Float)]
spheres = map makeSphere positions where
    makeSphere pos = (pos, Sphere pos 0.1)
    positions      = liftA3 V3 range range range
    range          = [-0.9,-0.45..0.9]

octree :: Octree Float (Sphere V3 Float)
octree = fromList 2 spheres
