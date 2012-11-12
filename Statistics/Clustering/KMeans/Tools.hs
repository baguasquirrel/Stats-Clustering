module Statistics.Clustering.KMeans.Tools where

import Statistics.Datasets.Clusters

import qualified Data.Foldable as Fl
import qualified Data.Traversable as Tv



pickInitialCenters :: (Tv.Traversable c, Vector v d, Ord d)
                   => Int
                   -> c v
                   -> [v]
pickInitialCenters k cs =
  iter []
  where
    iter l =
      case length l == k of
        True -> l
        False ->
          case mbBestNewCenter of
            Nothing -> l  -- empty collection, halt now, do not loop forever
            Just (v,minDist) ->
              iter (v:l')
          where
            (l',mbBestNewCenter) = Fl.foldl' pass (l,Nothing) cs

    pass ([],Nothing) v = ([v], Nothing)
    pass (ks,Nothing) v = (ks,  Just (v,minDist))
      where
        minDist = (minimum . map (euclideanDistance v)) ks

    pass (ks,Just (v',minDist')) v = (ks, Just (v'', minDist''))
      where
        minDist = (minimum . map (euclideanDistance v)) ks
        (v'', minDist'') =
          case minDist' < minDist of
            True -> (v,minDist)
            False -> (v',minDist')
