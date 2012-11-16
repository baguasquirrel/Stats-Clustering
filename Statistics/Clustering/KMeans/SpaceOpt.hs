{-# LANGUAGE BangPatterns #-}

module Statistics.Clustering.KMeans.SpaceOpt (kMeans, statsForResults) where

import Statistics.Datasets.Clusters

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Foldable as Fl
import qualified Data.Traversable as Tv




-- need to handle edge cases where:
-- the initialCenters list is zero length
-- the points list is zero length
kMeans :: (Tv.Traversable c, Vector v d, Vector acc d_acc, AccumVec v d acc d_acc, Ord acc, Ord d_acc, Fractional d, Fractional d_acc)
       => c v
       -> [acc]
       -> [acc]
kMeans pts initialCenters =
  step 1000 firstStepPts firstStepCenters
  where

    step :: (Tv.Traversable c, Vector v d, Vector acc d_acc, AccumVec v d acc d_acc, Ord acc, Ord d_acc, Fractional d, Fractional d_acc)
         => Int
         -> c (v,acc)
         -> [acc]
         -> [acc]
    step 0 !cs !centers = centers
    step n !cs !centers =
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
            centers' = fmap fst $ recomputeCenters centers cs'

        terminationCheckStep =
          (cs'', centers'', shouldContinue)
          where
            cs' = fmap (assignPointForCheck centers) cs
            centers' = recomputeCentersForCheck centers cs'

            shouldContinue = checkEarlyTermination cs' centers'

            cs'' = fmap (\(p,c,_) -> (p,c)) cs'
            centers'' = fmap (\(c,_,_) -> c) centers'


    firstStepPts = fmap (assignPoint initialCenters) pts
    firstStepCenters = fmap fst $ recomputeCenters initialCenters firstStepPts




assignPoint :: (Ord d_acc, Vector v d, Vector acc d_acc, AccumVec v d acc d_acc)
            => [acc]
            -> v
            -> (v,acc)
assignPoint centers p =
  (p, (fst . bestCenterWithDistance . toAccum) p)
  where
    bestCenterWithDistance pt =
      (c,d)
      where
        (c,d) = L.minimumBy compareDistance tupsCentersDistances
        compareDistance a b = compare (snd a) (snd b)
        tupsCentersDistances = zip centers (distances pt)

    distances pt = fmap (\p -> euclideanDistance p pt) centers


recomputeCenters :: (Fl.Foldable t, AccumVec v d acc d_acc, Ord acc, Fractional d_acc)
                 => [acc]
                 -> t (v,acc)
                 -> [(acc,d_acc)]
recomputeCenters centers pts =
  (calculateNewCentersFromAccs . (vectorSumAccsForCenters centers)) pts
  where

    calculateNewCentersFromAccs :: (Vector acc d_acc, Ord acc, Fractional d_acc)
                                => M.Map acc (acc, Int, d_acc)
                                -> [(acc,d_acc)]
    calculateNewCentersFromAccs = reduceAccs . M.toList
      where
        reduceAccs = fmap reduceAccToCenter
        reduceAccToCenter = normalizeVectorSum . getVectorSums
        normalizeVectorSum (v_acc, n, d_acc) = (scale (1 / fromIntegral n) v_acc, d_acc / fromIntegral n)
        getVectorSums = snd

    vectorSumAccsForCenters :: (Fl.Foldable f, Vector v d, Vector acc d_acc, Ord acc, AccumVec v d acc d_acc, Fractional d_acc)
                            => [acc]
                            -> f (v, acc)
                            -> M.Map acc (acc, Int, d_acc)
    vectorSumAccsForCenters centers pts =
      Fl.foldl' updateCenter initAccs pts
      where
        updateCenter !accs (p,c) =
          M.adjust (helper p') c accs
          where
            helper p' !(acc, n, d_acc) = (addVectors p' acc, n+1, d + d_acc)
            d = euclideanDistance p' c
            p' = toAccum p

        initAccs = (M.fromList . fmap initSums) centers
          where
            initSums c = (c, (zeroVector,0,0))


checkEarlyTermination :: (Vector v d, Vector acc d_acc, AccumVec v d acc d_acc, Tv.Traversable c, Ord acc, Ord d_acc, Fractional d_acc)
                      => c (v, acc, acc)  -- ^ (the point, its cluster, its previous cluster)
                      -> [(acc, d_acc, d_acc)]        -- ^ (center, average distance from center to its points, old average distance from previous iteration)
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


assignPointForCheck :: (Ord d_acc, Vector v d, Vector acc d_acc, AccumVec v d acc d_acc)
                    => [acc]
                    -> (v,acc)
                    -> (v,acc,acc)
assignPointForCheck centers (p,c_old) =
  (p,c,c_old)
  where
    -- d_old = euclideanDistance p c_old
    p' = toAccum p
    (c,d) = bestCenterWithDistance p'

    bestCenterWithDistance pt =
      (c,d)
      where
        (c,d) = L.minimumBy compareDistance tupsCentersDistances
        compareDistance a b = compare (snd a) (snd b)
        tupsCentersDistances = zip centers (distances pt)

    distances pt = fmap (\c -> euclideanDistance c pt) centers


recomputeCentersForCheck :: (Vector v d, Vector acc d_acc, AccumVec v d acc d_acc, Ord acc, Fl.Foldable f, Num d_acc, Fractional d_acc)
                         => [acc]                -- ^ centers
                         -> f (v, acc, acc)      -- ^ (point, assiged center, old assigned center)
                         -> [(acc,d_acc,d_acc)]  -- ^ (center, average distance to points assigned to this center, old average distance )
recomputeCentersForCheck centers pts =
  (calculateNewCentersFromAccs . (vectorSumAccsForCenters centers)) pts
  where
    calculateNewCentersFromAccs :: (Vector acc d_acc, Ord acc, Fractional d_acc)
                                => M.Map acc (acc, Int, d_acc, d_acc)
                                -> [(acc, d_acc, d_acc)]
    calculateNewCentersFromAccs = reduceAccs . M.toList
      where
        reduceAccs = fmap reduceAccToCenter
        reduceAccToCenter = normalizeSums . getVectorSums
        getVectorSums = snd

        normalizeSums (v_acc, n, d_acc, d_old_acc) =
          (center,avgDist,oldAvgDist)
          where
            center = scale (1 / fromIntegral n) v_acc
            avgDist = d_acc / (fromIntegral n)
            oldAvgDist = d_old_acc / (fromIntegral n)

    vectorSumAccsForCenters :: (Fl.Foldable f, Vector v d, Vector acc d_acc, AccumVec v d acc d_acc, Ord acc, Num d_acc)
                            => [acc]
                            -> f (v, acc, acc)
                            -> M.Map acc (acc, Int, d_acc, d_acc)
    vectorSumAccsForCenters centers pts =
      Fl.foldl' updateCenter initAccs pts
      where
        updateCenter accs (v, c, c_old) =
          M.adjust (helper v') c accs
          where
            v' = toAccum v
            d = euclideanDistance v' c
            d_old = euclideanDistance v' c_old

            helper v (v_acc, n, d_acc, d_old_acc) =
              (v_acc', n', d_acc', d_old_acc')
              where
                v_acc' = addVectors v v_acc
                n'     = n+1
                d_acc'     = d + d_acc
                d_old_acc' = d_old + d_old_acc

        initAccs = (M.fromList . fmap initSums) centers
          where
            initSums c = (c, (zeroVector,0,0,0))




statsForResults :: (Vector v d_v, Vector acc d_acc, AccumVec v d acc d_acc, Tv.Traversable c, Ord d, Ord acc, Ord d_acc, Fractional d_acc)
                => c v   -- ^ (the point, its cluster, its previous cluster, its distance to its cluster, its previous distance to its cluster)
                -> [acc]   -- ^ (center, average distance from center to its points, old average distance from previous iteration)
                -> Maybe ((d_acc,d_acc,d_acc,d_acc), (d_acc,d_acc,d_acc))
statsForResults cs centers =
  do  -- compute point stats
      let cs' = fmap (assignPoint centers) cs

          (totalCount, distTotalAcc, minmax) =
            Fl.foldl' tallyCluster (0,0,Nothing) cs'
            where
              tallyCluster (totalCount, distTotalAcc, Just (distMin, distMax)) (p,c) =
                (totalCount + 1, distTotalAcc + d, Just (min distMin d, max distMax d))
                where
                  d = euclideanDistance (toAccum p) c

              tallyCluster (totalCount, distTotalAcc, Nothing) (p,c) =
                (totalCount + 1, distTotalAcc + d, Just (d,d))
                where
                  d = euclideanDistance (toAccum p) c

      (pointDistMin, pointDistMax) <- minmax

      let pointDistMean = distTotalAcc / fromIntegral totalCount

      -- compute center stats
      let centersWithDistances = recomputeCenters centers cs'
          centersDistances = L.sort $ fmap snd centersWithDistances

          firstToMaybe [] = Nothing
          firstToMaybe (x:xs) = Just x

          lastToMaybe [] = Nothing
          lastToMaybe (x:[]) = Just x
          lastToMaybe (x:xs) = lastToMaybe xs

      centerAvgDistMin <- firstToMaybe centersDistances
      centerAvgDistMax <- lastToMaybe centersDistances

      let centerAvgDistMean = (L.sum centersDistances) / (fromIntegral numCenters)
          centerAvgDistMedian = centersDistances L.!! (numCenters `div` 2)
          numCenters = length centersDistances

      return ((centerAvgDistMin, centerAvgDistMean, centerAvgDistMedian, centerAvgDistMax), (pointDistMin, pointDistMean, pointDistMax))



