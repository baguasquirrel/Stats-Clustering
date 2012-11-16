{-# LANGUAGE FunctionalDependencies #-}

module Statistics.Types.PointVec where
  

class PointVec ptTy distTy | ptTy -> distTy where
  euclideanDistance :: ptTy -> ptTy -> distTy
  euclideanDistance a b = norm $ diffVectors a b

  addVectors :: ptTy -> ptTy -> ptTy
  diffVectors :: ptTy -> ptTy -> ptTy

  norm :: ptTy -> distTy
  norm2 :: ptTy -> distTy

  scale :: distTy -> ptTy -> ptTy

  unitVector :: ptTy
  zeroVector :: ptTy


class (PointVec acc d_acc) => ToPointVec v acc d_acc | v -> acc where
  toPointVec :: v -> acc



{-
class PointVec ptTy unitsTy distTy | ptTy -> distTy, ptTy -> unitsTy where
  pmap :: (unitsTy -> unitsTy) -> ptTy -> ptTy
  pzip :: (unitsTy -> unitsTy -> unitsTy) -> ptTy -> ptTy -> ptTy
  pfold :: (distTy -> unitsTy -> distTy) -> distTy -> ptTy -> distTy
  ppack :: [unitsTy] -> (ptTy, [unitsTy])
  punpack :: ptTy -> [unitsTy]
  ppromote :: unitsTy -> ptTy

  unitsToDist :: ptTy -> unitsTy -> distTy
  distToUnits :: ptTy -> distTy -> unitsTy

  pdimension :: ptTy -> Int
  pindex :: ptTy -> Int -> unitsTy

  euclideanDistance :: (Num unitsTy, Floating distTy) => ptTy -> ptTy -> distTy
  euclideanDistance a b = norm $ diffVectors a b

  addVectors :: (Num unitsTy) => ptTy -> ptTy -> ptTy
  addVectors = pzip (+)

  diffVectors :: (Num unitsTy) => ptTy -> ptTy -> ptTy
  diffVectors = pzip (-)

  norm :: (Num unitsTy, Floating distTy) => ptTy -> distTy
  norm = sqrt . norm2

  norm2 :: (Num unitsTy, Num distTy) => ptTy -> distTy
  norm2 p = pfold (\acc -> \i -> (unitsToDist p i) * (unitsToDist p i) + acc) 0 p

  scale :: (Num distTy) => distTy -> ptTy -> ptTy
  scale s p = pmap ((distToUnits p) . ((*) s) . (unitsToDist p)) p

  unitVector :: (Num unitsTy) => ptTy
  unitVector = ppromote 1

  zeroVector :: (Num unitsTy) => ptTy
  zeroVector = ppromote 0
 

class (PointVec p u d, PointVec pa ua da) => PointAccum p u d pa ua da | p -> pa where
  toAccum :: p -> pa
  toAccumD :: d -> da
-}
