module Statistics.TestKMeans where

import Statistics.Datasets.SampleVectorDefinitions
import Statistics.Datasets.Clusters
import Statistics.Clustering.KMeans.Tools
import qualified Statistics.Clustering.KMeans.SpaceOpt as KMeans

import qualified Data.List as L

-- from the package mersenne-random-pure64
import System.Random.Mersenne.Pure64

-- from the package normaldistribution
import Data.Random.Normal

-- from random-shuffle
import System.Random.Shuffle

-- from lens, provides the TraversableWithIndex class
import Control.Lens.WithIndex


makeTestSet :: IO ([FVec3],Int,[FVec3])
makeTestSet = do
  mt <- newPureMT

  let nclusters = 5
      pointsPerCluster = 20
      clusterSize = 10.0
      clusterPadding = 10
      clusterStdDev = clusterSize / 3
      (actualClusters,cs) = clusters (FVec3 0 0 0, FVec3 100 100 100) clusterStdDev clusterPadding nclusters pointsPerCluster mt
      actualNumClusters = length actualClusters

  mtShuffle <- newPureMT

  let -- flattened list of points
      psf = concat cs

      -- randomized dataset
      psfr = shuffle' psf (actualNumClusters * pointsPerCluster) mtShuffle

  return (actualClusters, actualNumClusters, psfr)


testScript :: IO ()
testScript = do
  (actualCenters, actualNumClusters, shuffledPts) <- makeTestSet
  -- let naiveInitialClusters = take actualNumClusters shuffledPts
      
  let initialClusters = pickInitialCenters actualNumClusters shuffledPts
      initialClusters' = fmap toAccum initialClusters
      determinedClusters100 = KMeans.kMeans shuffledPts initialClusters'

  dumpCSV2 (L.sort determinedClusters100) "estimatedCenters.csv"
  dumpCSV2 (L.sort actualCenters) "actualCenters.csv"

  return ()
