{-# LANGUAGE FunctionalDependencies #-}

module Statistics.Datasets.Clusters 
  ( Vector(..)
  , AccumVec(..)
  , CSV_Output(..)
  , dumpCSV
  , dumpCSV2
  , RandomGaussian(..)
  , clusters
  , clusters'
  ) where

import System.Random
import qualified Data.List as L
import qualified Data.Traversable as Tv
import qualified Data.Vector as V

import System.IO


class Vector ptTy distTy | ptTy -> distTy where
  euclideanDistance :: ptTy -> ptTy -> distTy
  euclideanDistance a b = norm $ diffVectors a b

  addVectors :: ptTy -> ptTy -> ptTy
  diffVectors :: ptTy -> ptTy -> ptTy

  norm :: ptTy -> distTy
  norm2 :: ptTy -> distTy

  scale :: ptTy -> distTy -> ptTy

  -- useful for determining the centroid of a list of points.
  -- you accumulate the sum of the points, and then divide by
  -- the number of points
  divideByIntegral :: (Integral a) => ptTy -> a -> ptTy

  unitVector :: ptTy
  zeroVector :: ptTy





class (Vector v d, Vector acc d_acc) => AccumVec v d acc d_acc | v -> acc, acc -> v, v -> d, d -> v, v -> d_acc, d -> d_acc where
  toAccum :: v -> acc
  toVector :: acc -> v
  toAccumD :: d -> d_acc
  toVectorD :: d_acc -> d






class CSV_Output a where
  toCSV :: a -> String  

dumpCSV :: (CSV_Output a, Tv.Traversable t) => t a -> FilePath -> IO ()
dumpCSV cs fp =
  do h <- openFile fp WriteMode
     Tv.mapM (\c -> hPutStrLn h (toCSV c)) cs
     hClose h

dumpCSV2 :: (CSV_Output a) => [a] -> FilePath -> IO ()
dumpCSV2 cs fp =
  do h <- openFile fp WriteMode
     mapM_ (\c -> hPutStrLn h (toCSV c)) cs
     hClose h



randomRgs :: (RandomGen g, Random v) => (v,v) -> g -> [(v,g)]
randomRgs bounds g =
  (\(v,g') -> (v,g') : randomRgs bounds g') (randomR bounds g)


class RandomGaussian v where
  normal :: (RandomGen g) => g -> (v, g)

  normals :: (RandomGen g) => g -> [v]
  normals g = (\(x,g') -> x : normals g') (normal g)

  normalD :: (RandomGen g, Vector v d) => (v,d) -> g -> (v, g)
  normalD (v,d) g =
    (v'',g')
    where
      (v',g') = normal g
      v'' = addVectors v $ scale v' d

  normalDs :: (RandomGen g, Vector v d) => (v,d) -> g -> [v]
  normalDs vd g = (\(x,g') -> x : normalDs vd g') (normalD vd g)

  normalDgs :: (RandomGen g, Vector v d) => (v,d) -> g -> [(v,g)]
  normalDgs vd g = (\(x,g') -> (x,g') : normalDgs vd g') (normalD vd g)






clusters :: (RandomGen g, Vector v d, Random v, RandomGaussian v, Num d, Ord d)
         => (v,v)  -- ^ the (lower,upper) corners of the bounding box where we want our clusters
         -> d      -- ^ the standard deviation of our clusters
         -> d      -- ^ padding space between clusters
         -> Int    -- ^ number of clusters we desire. the actual number of clusters may be smaller, but always greater than zero
         -> Int    -- ^ number of points per cluster
         -> g      -- ^ our random source

         -> ([v], [[v]])  -- ^ (the cluster centers, the lists of points, grouped by cluster)

clusters bounds stddev padding nclusters pointsPerCluster g =
  (cs, clusters' stddev padding pointsPerCluster cs g')
  where
    (cs,g') = createClusterCenters bounds stddev padding nclusters g





clusters' :: (RandomGen g, Vector v d, Random v, RandomGaussian v, Num d, Ord d)
          => d      -- ^ the standard deviation of our clusters
          -> d      -- ^ padding space between clusters
          -> Int    -- ^ number of clusters we desire. the actual number of clusters may be smaller, but always greater than zero
          -> [v]    -- ^ the cluster centers
          -> g      -- ^ our random source

          -> [[v]]  -- ^ the lists of points, grouped by cluster

clusters' stddev padding pointsPerCluster cs g =
  map fst $ generateClustersFromCenters g cs
  where
    generateClustersFromCenters g [] = []
    generateClustersFromCenters g (c:cs) =
      (ps,g') : (generateClustersFromCenters g' cs)
      where
        ps = map fst ps'
        g' = snd $ last ps'
        ps' = take pointsPerCluster $ normalDgs (c,stddev) g          





createClusterCenters :: (RandomGen g, Vector v d, Random v, Num d, Ord d)
                     => (v,v) -- ^ bounds
                     -> d     -- ^ std dev
                     -> d     -- ^ padding
                     -> Int   -- ^ number of clusters
                     -> g     -- ^ random gen
                     -> ([v],g)
createClusterCenters bounds stddev padding nclusters g =
  (map fst ps, snd $ last ps')
    where
      -- ps = filter (\currPt -> (not $ any (pointIsClose currPt) us)) us
      ps = L.nubBy (\(a,_) -> \(b,_) -> (pointIsClose a b)) ps'

      ps' = take nclusters $ randomRgs bounds g

      pointIsClose x y =
        euclideanDistance x y < minDistance

      minDistance = (stddev * 3) + padding


