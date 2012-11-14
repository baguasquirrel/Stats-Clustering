{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Statistics.Clustering.KMeans.Generic (kMeans, statsForResults) where

import Statistics.Datasets.Clusters
import qualified Statistics.Clustering.KMeans.SpaceOpt as SO

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Foldable as Fl
import qualified Data.Traversable as Tv




instance (Vector v d) => AccumVec v d v d where
  toAccum = id
  toAccumD = id


kMeans :: (Tv.Traversable c, Vector v d, Ord d, Ord v, Fractional d)
       => c v
       -> [v]
       -> [v]
kMeans pts initialCenters = SO.kMeans pts initialCenters


statsForResults :: (Vector v d, Tv.Traversable c, Ord d, Ord v, Fractional d)
                => c v   -- ^ (the point, its cluster, its previous cluster, its distance to its cluster, its previous distance to its cluster)
                -> [v]   -- ^ (center, average distance from center to its points, old average distance from previous iteration)
                -> Maybe ((d,d,d,d), (d,d,d))
statsForResults cs centers = SO.statsForResults cs centers







