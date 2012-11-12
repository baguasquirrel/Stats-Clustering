
module Statistics.Clustering.KMeans where

import Statistics.Datasets.Clusters

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Foldable as Fl
import qualified Data.Traversable as Tv




-- need to handle edge cases where:
-- the initialCenters list is zero length
-- the points list is zero length
kMeans :: (Tv.Traversable c, Vector v d, Vector acc d_acc, AccumVec v d acc d_acc, Ord d, Ord v, Fractional d, Fractional d_acc)
       => c v
       -> [v]
       -> [v]
kMeans pts initialCenters =
  step 1000 firstStepPts firstStepCenters
  where

    step :: (Vector v d, Vector acc d_acc, Tv.Traversable c, AccumVec v d acc d_acc, Ord v, Ord d, Fractional d, Fractional d_acc)
         => Int
         -> c (v,v)
         -> [v]
         -> [v]
    step 0 cs centers = centers
    step n cs centers =
      nextIter shouldContinue
      where
        nextIter True =
          step (n-1) cs' centers'
        nextIter False =
          centers'

        (cs', centers', shouldContinue) =
          case n > 20 && n `mod` 20 == 0 of
            True -> regularStep
            False -> terminationCheckStep

        regularStep =
          (cs', centers', True)
          where
            getPoint (p,c) = p
            cs' = fmap ((assignPoint centers) . getPoint) cs
            centers' = recomputeCenters centers cs'

        terminationCheckStep =
          (cs'', centers'', shouldContinue)
          where
            cs' = fmap (assignPointForCheck centers) cs
            centers' = recomputeCentersForCheck centers cs'

            shouldContinue = checkEarlyTermination cs' centers'

            cs'' = fmap (\(p,c,_) -> (p,c)) cs'
            centers'' = map (\(c,_,_) -> c) centers'


    firstStepPts = fmap (assignPoint initialCenters) pts
    firstStepCenters = recomputeCenters initialCenters firstStepPts


    assignPoint :: (Ord d, Vector v d)
                => [v]
                -> v
                -> (v,v)
    assignPoint centers p =
      (p,c)
      where
        (c,_) = bestCenterWithDistance p

        bestCenterWithDistance pt =
          (c,d)
          where
            (c,d) = L.minimumBy compareDistance tupsCentersDistances
            compareDistance a b = compare (snd a) (snd b)
            tupsCentersDistances = zip centers (distances pt)

        distances pt = map (\p -> euclideanDistance p pt) centers


    recomputeCenters :: (Ord v, Fl.Foldable t, AccumVec v d acc d_acc)
                     => [v]
                     -> t (v, v)
                     -> [v]
    recomputeCenters centers pts =
      (calculateNewCentersFromAccs . (vectorSumAccsForCenters centers)) pts
      where

        calculateNewCentersFromAccs :: (Vector v d, Vector acc d_acc, Ord v, AccumVec v d acc d_acc)
                                    => M.Map v (acc,Int)
                                    -> [v]
        calculateNewCentersFromAccs = reduceAccs . M.toList
          where
            reduceAccs = map reduceAccToCenter
            reduceAccToCenter = normalizeVectorSum . getVectorSums
            normalizeVectorSum (v,n) = toVector $ divideByIntegral v n
            getVectorSums = snd

        vectorSumAccsForCenters :: (Fl.Foldable f, Vector v d, Vector acc d_acc, Ord v, AccumVec v d acc d_acc)
                                => [v]
                                -> f (v, v)
                                -> M.Map v (acc, Int)
        vectorSumAccsForCenters centers pts =
          Fl.foldl' updateCenter initAccs pts
          where
            updateCenter accs (p,c) =
              M.adjust (helper p) c accs
              where
                helper p (acc, n) = (addVectors (toAccum p) acc, n+1)

            initAccs = (M.fromList . map initSums) centers
              where
                initSums c = (c, (zeroVector,0))


    checkEarlyTermination :: (Vector v d, Vector acc d_acc, AccumVec v d acc d_acc, Tv.Traversable c, Ord d, Ord v, Fractional d)
                          => c (v, v, v)   -- ^ (the point, its cluster, its previous cluster)
                          -> [(v,d,d)]     -- ^ (center, average distance from center to its points, old average distance from previous iteration)
                          -> Bool
    checkEarlyTermination cs centers =
      (clustersNotSignificantlyMoving && clustersNotSignificantlyReassigned)
      where
        clustersNotSignificantlyReassigned =
          (changedCount / totalCount) < (1 / 100)

        (changedCount, totalCount) =
          Fl.foldl' tallyCluster (0,0) cs
          where
            tallyCluster (changedCount, totalCount) c =
              (changedCount + clusterChanged c, totalCount + 1)
              where
                clusterChanged (_,c,c_old) =
                  case c == c_old of
                    True -> 1
                    False -> 0

        clustersNotSignificantlyMoving =
          all checkClusterMovement centers

        checkClusterMovement (c,avgDist,oldAvgDist) =
          percentageChange < (5 / 100)
          where
            percentageChange = 
              (abs (avgDist - oldAvgDist)) / oldAvgDist


    statsForResults :: (Vector v d, Vector acc d_acc, AccumVec v d acc d_acc, Tv.Traversable c, Ord d, Ord v, Ord d_acc, Fractional d, Fractional d_acc)
                    => c (v, v, v, d, d)   -- ^ (the point, its cluster, its previous cluster, its distance to its cluster, its previous distance to its cluster)
                    -> [(v,d,d)]           -- ^ (center, average distance from center to its points, old average distance from previous iteration)
                    -> Maybe ((d,d,d,d), (d,d,d))
    statsForResults cs centers =
      do  -- compute point stats
          let centersDistances = L.sort $ L.map (\(_,d,_) -> d) centers

              (totalCount, distTotalAcc, minmax) =
                Fl.foldl' tallyCluster (0,0,Nothing) cs
                where
                  tallyCluster (totalCount, distTotalAcc, Just (distMin, distMax)) (p,c,c_old,d,d_old) =
                    (totalCount + 1, distTotalAcc + toAccumD d, Just (min distMin d, max distMax d))

                  tallyCluster (totalCount, distTotalAcc, Nothing) (p,c,c_old,d,d_old) =
                    (totalCount + 1, distTotalAcc + toAccumD d, Just (d,d))

          (pointDistMin, pointDistMax) <- minmax

          let pointDistMean = toVectorD (distTotalAcc / fromIntegral totalCount)

          -- compute center stats
          let centersDistances = L.sort $ L.map (\(_,d,_) -> d) centers

              firstToMaybe [] = Nothing
              firstToMaybe (x:xs) = Just x

              lastToMaybe [] = Nothing
              lastToMaybe (x:[]) = Just x
              lastToMaybe (x:xs) = lastToMaybe xs

          centerAvgDistMin <- firstToMaybe centersDistances
          centerAvgDistMax <- lastToMaybe centersDistances

          let centerAvgDistMean = toVectorD $ (sum $ map toAccumD centersDistances) / (fromIntegral numCenters)
              centerAvgDistMedian = centersDistances !! (numCenters `div` 2)
              numCenters = length centersDistances

          return ((centerAvgDistMin, centerAvgDistMean, centerAvgDistMedian, centerAvgDistMax), (pointDistMin, pointDistMean, pointDistMax))


    assignPointForCheck :: (Ord d, Vector v d)
                        => [v]
                        -> (v,v)
                        -> (v,v,v)
    assignPointForCheck centers (p,c_old) =
      (p,c,c_old)
      where
        -- d_old = euclideanDistance p c_old

        (c,d) = bestCenterWithDistance p

        bestCenterWithDistance pt =
          (c,d)
          where
            (c,d) = L.minimumBy compareDistance tupsCentersDistances
            compareDistance a b = compare (snd a) (snd b)
            tupsCentersDistances = zip centers (distances pt)

        distances pt = map (\c -> euclideanDistance c pt) centers


    recomputeCentersForCheck :: (Vector v d, Vector acc d_acc, AccumVec v d acc d_acc, Ord v, Fractional d, Fl.Foldable f, Num d_acc, Fractional d_acc)
                             => [v]
                             -> f (v, v, v)
                             -> [(v,d,d)]
    recomputeCentersForCheck centers pts =
      (calculateNewCentersFromAccs . (vectorSumAccsForCenters centers)) pts
      where
        calculateNewCentersFromAccs :: (Vector v d, Vector acc d_acc, AccumVec v d acc d_acc, Ord v, Fractional d_acc)
                                    => M.Map v (acc, Int, d_acc, d_acc)
                                    -> [(v, d, d)]
        calculateNewCentersFromAccs = reduceAccs . M.toList
          where
            reduceAccs = map reduceAccToCenter
            reduceAccToCenter = normalizeSums . getVectorSums
            getVectorSums = snd

            normalizeSums (v_acc, n, d_acc, d_old_acc) =
              (center,avgDist,oldAvgDist)
              where
                center = toVector $ divideByIntegral v_acc n
                avgDist = toVectorD (d_acc / (fromIntegral n))
                oldAvgDist = toVectorD (d_old_acc / (fromIntegral n))


    vectorSumAccsForCenters :: (Fl.Foldable f, Vector v d, Vector acc d_acc, AccumVec v d acc d_acc, Ord v, Num d_acc)
                            => [v]
                            -> f (v, v, v)
                            -> M.Map v (acc, Int, d_acc, d_acc)
    vectorSumAccsForCenters centers pts =
      Fl.foldl' updateCenter initAccs pts
      where
        updateCenter accs (v, c, c_old) =
          M.adjust (helper v) c accs
          where
            d = euclideanDistance v c
            d_old = euclideanDistance v c_old

            helper v (v_acc, n, d_acc, d_old_acc) =
              (v_acc', n', d_acc', d_old_acc')
              where
                v_acc' = addVectors (toAccum v) v_acc
                n'     = n+1
                d_acc'     = (toAccumD d) + d_acc
                d_old_acc' = (toAccumD d_old) + d_old_acc

        initAccs = (M.fromList . map initSums) centers
          where
            initSums c = (c, (zeroVector,0,0,0))


