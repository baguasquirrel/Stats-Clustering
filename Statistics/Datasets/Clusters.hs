{-# LANGUAGE FunctionalDependencies #-}

module Statistics.Datasets.Clusters 
  ( clusters
  , clusters'
  ) where

import System.Random
import qualified Data.List as L
import qualified Data.Traversable as Tv
import qualified Data.Vector as V

import Statistics.Types.Debug
import Statistics.Types.PointVec
import Statistics.Distributions.Gaussian






randomRgs :: (RandomGen g, Random v) => (v,v) -> g -> [(v,g)]
randomRgs bounds g =
  (\(v,g') -> (v,g') : randomRgs bounds g') (randomR bounds g)




clusters :: (RandomGen g, PointVec v d, Random v, UniformGaussianValues v d, Num d, Ord d)
         => (v,v)  -- ^ the (lower,upper) corners of the bounding box where we want our clusters
         -> d      -- ^ the standard deviation of our clusters
         -> d      -- ^ padding space between clusters
         -> Int    -- ^ number of clusters we desire. the actual number of clusters may be smaller, but always greater than zero
         -> Int    -- ^ number of points per cluster
         -> g      -- ^ our random source

         -> ([v], [[v]])  -- ^ (the cluster centers, the lists of points, grouped by cluster)

clusters bounds stddev padding nclusters pointsPerCluster g =
  (cs, clusters' stddev pointsPerCluster cs g')
  where
    (cs,g') = createClusterCenters bounds padding nclusters g





clusters' :: (RandomGen g, PointVec v d, Random v, UniformGaussianValues v d, Num d, Ord d)
          => d      -- ^ the standard deviation of our clusters
          -> Int    -- ^ number of clusters we desire. the actual number of clusters may be smaller, but always greater than zero
          -> [v]    -- ^ the cluster centers
          -> g      -- ^ our random source

          -> [[v]]  -- ^ the lists of points, grouped by cluster

clusters' stddev pointsPerCluster cs g =
  map fst $ generateClustersFromCenters g cs
  where
    generateClustersFromCenters g [] = []
    generateClustersFromCenters g (c:cs) =
      (ps,g') : (generateClustersFromCenters g' cs)
      where
        ps = map fst ps'
        g' = snd $ last ps'
        ps' = take pointsPerCluster $ normalDgs (c,stddev) g          





createClusterCenters :: (RandomGen g, PointVec v d, Random v, Num d, Ord d)
                     => (v,v) -- ^ bounds
                     -> d     -- ^ padding
                     -> Int   -- ^ number of clusters
                     -> g     -- ^ random gen
                     -> ([v],g)
createClusterCenters bounds padding nclusters g =
  (map fst ps, snd $ last ps')
    where
      -- ps = filter (\currPt -> (not $ any (pointIsClose currPt) us)) us
      ps = L.nubBy (\(a,_) -> \(b,_) -> (pointIsClose a b)) ps'

      ps' = take nclusters $ randomRgs bounds g

      pointIsClose x y =
        euclideanDistance x y < padding



