{-# LANGUAGE MultiParamTypeClasses #-}

module Statistics.Datasets.SampleVectorDefinitions where

import Statistics.Datasets.Clusters
import GHC.Float
import System.Random

-- from the package normaldistribution
import qualified Data.Random.Normal as N


data FVec3 =
  FVec3 { fv3x, fv3y, fv3z :: {-# UNPACK #-} !Float }
  deriving (Eq, Show, Ord)

data DVec3 =
  DVec3 { dv3x, dv3y, dv3z :: {-# UNPACK #-} !Double }
  deriving (Eq, Show, Ord)


instance RandomGaussian FVec3 where
  normal g0 =
    (FVec3 x y z, g3)
    where
      (x,g1) = N.normal g0
      (y,g2) = N.normal g1
      (z,g3) = N.normal g2

instance Random FVec3 where
  randomR (FVec3 lx ly lz, FVec3 hx hy hz) g0 =
    (FVec3 xr yr zr, g3)
    where
      (xr,g1) = randomR (lx,hx) g0
      (yr,g2) = randomR (ly,hy) g1
      (zr,g3) = randomR (lz,hz) g2

  random g0 =
    (FVec3 n1 n2 n3, g3)
    where
      (n1,g1) = random g0
      (n2,g2) = random g1
      (n3,g3) = random g2


instance Vector FVec3 Float where
  addVectors (FVec3 x1 y1 z1) (FVec3 x2 y2 z2) =
    FVec3 (x1+x2) (y1+y2) (z1+z2)

  diffVectors (FVec3 x1 y1 z1) (FVec3 x2 y2 z2) =
    FVec3 (x1-x2) (y1-y2) (z1-z2)

  norm2 (FVec3 x y z) =
    xd + yd + zd
    where
      xd = x * x
      yd = y * y
      zd = z * z

  norm = sqrt . norm2

  unitVector = FVec3 1 1 1
  zeroVector = FVec3 0 0 0

  scale (FVec3 x y z) c =
    FVec3 (c * x) (c * y) (c * z)

  divideByIntegral (FVec3 x y z) c = 
    FVec3 (x / c') (y / c') (z / c')
    where
      c' = fromIntegral c


instance Vector DVec3 Double where
  addVectors (DVec3 x1 y1 z1) (DVec3 x2 y2 z2) =
    DVec3 (x1+x2) (y1+y2) (z1+z2)

  diffVectors (DVec3 x1 y1 z1) (DVec3 x2 y2 z2) =
    DVec3 (x1-x2) (y1-y2) (z1-z2)

  norm2 (DVec3 x y z) =
    xd + yd + zd
    where
      xd = x * x
      yd = y * y
      zd = z * z

  norm = sqrt . norm2

  unitVector = DVec3 1 1 1
  zeroVector = DVec3 0 0 0

  scale (DVec3 x y z) c =
    DVec3 (c * x) (c * y) (c * z)

  divideByIntegral (DVec3 x y z) c = 
    DVec3 (x / c') (y / c') (z / c')
    where
      c' = fromIntegral c


instance Random DVec3 where
  randomR (DVec3 lx ly lz, DVec3 hx hy hz) g0 =
    (DVec3 xr yr zr, g3)
    where
      (xr,g1) = randomR (lx,hx) g0
      (yr,g2) = randomR (ly,hy) g1
      (zr,g3) = randomR (lz,hz) g2

  random g0 =
    (DVec3 n1 n2 n3, g3)
    where
      (n1,g1) = random g0
      (n2,g2) = random g1
      (n3,g3) = random g2


instance AccumVec FVec3 Float DVec3 Double where
  toAccum (FVec3 x y z) = DVec3 (float2Double x) (float2Double y) (float2Double z)
  toAccumD = float2Double


instance CSV_Output FVec3 where
  toCSV (FVec3 x y z) =
    (show x) ++ "," ++ (show y) ++ "," ++ (show z) ++ "\r"


instance CSV_Output DVec3 where
  toCSV (DVec3 x y z) =
    (show x) ++ "," ++ (show y) ++ "," ++ (show z) ++ "\r"


