{-# LANGUAGE FunctionalDependencies #-}

module Statistics.Gaussian (pdf, std_pdf, nonnormalized_pdf, nonnormalized_inverse_pdf, erfc, zigTable, ziggurat, RandomGaussian(..), histogram) where

import qualified Data.Vector as V
import qualified System.Random as R
import qualified Data.Foldable as Fl
import qualified Data.IntMap as IM



-- ^ calculate the gaussian pdf function at some position x
pdf :: (Floating a)
    => (a,a)   -- ^ (mean, sigma)
    -> a       -- ^ a position x
    -> a       -- ^ the pdf of the gaussian curve at x
pdf (mean,sigma) x =
  b / a
  where
    d = x - mean
    b = exp $ (-0.5 * d * d) / (sigma * sigma)
    a = (sqrt $ 2 * pi) * sigma


-- ^ calculate the gaussian pdf at some position x, with a mean of 0.0, sigma of 1.0
std_pdf :: (Floating a) => a -> a
std_pdf x =
  b / a
  where
    b = exp $ (-0.5 * x * x)
    a = (sqrt $ 2 * pi)


nonnormalized_pdf :: (Floating a)
                  => a
                  -> a
nonnormalized_pdf x = exp (-0.5 * x * x)

area_of_tail_pdf :: (Floating a) => a -> a
area_of_tail_pdf x = (sqrt (pi * 0.5)) * (erfc (x * 0.7071067811865475))

nonnormalized_inverse_pdf :: (Floating a) => a -> a
nonnormalized_inverse_pdf y = sqrt (-2.0 * log y)


erfc :: (Floating a) => a -> a
erfc x =
  1 - (1 / b16)
  where
    b16 = b8 * b8
    b8 = b4 * b4
    b4 = b2 * b2
    b2 = b * b
    b = 1 + 0.0705230784 * x1 + 0.0422820123 * x2 + 0.0092705272 * x3 + 0.0001520143 * x4 + 0.0002765672 * x5 + 0.0000430638 * x6
    x1 = x
    x2 = x1 * x
    x3 = x2 * x
    x4 = x3 * x
    x5 = x4 * x
    x6 = x5 * x



--

zigTableSize = 128

zigX0 :: (Floating a) => a
zigX0 = 3.442619855899

zigA :: (Floating a) => a
zigA = 9.91256303526217e-3

zigTable :: (Floating a) => V.Vector (a,a)
zigTable =
  (V.fromList $ (take zigTableSize zigList) ++ [(0.0, 1.0)])
  where
    zigList = (initX0, initY0) : (initX1, initY1) : f (initX1, initY1)
    initX0 = zigX0
    initY0 = nonnormalized_pdf zigX0
    initX1 = initX0
    initY1 = initY0 + zigA / initX1

    f (x_p, y_p) =
      p_c : (f p_c)
      where
        p_c = (x_c, y_c)
        x_c = nonnormalized_inverse_pdf y_p
        y_c = y_p + zigA / x_c

zigTableFloats :: V.Vector (Float,Float)
zigTableFloats = zigTable

zigTableDoubles :: V.Vector (Double,Double)
zigTableDoubles = zigTable

ziggurat :: (Floating p, Ord p, R.Random p, R.RandomGen g)
         => V.Vector (p,p)
         -> g
         -> (p,g)
ziggurat table g0 =
  checkXStep
  where
    -- choose random layer
    (l,g1) = R.randomR (0,zigTableSize-1) g0
    (x_l,y_l) = table V.! l
    (x_l_plus1,y_l_plus1) = table V.! (l+1)

    -- choose X
    (u0,g2) = R.randomR (-1,1) g1
    x = u0 * x_l

    -- does x fall inside the rectangle?
    checkXStep =
      case abs x < x_l_plus1 of
        True -> (x,g2)
        False ->
          case l == 0 of
            True -> fallbackToTailStep g2
            False -> checkYStep

    -- x did not fall outside the rectangle, and it was
    -- not the bottom layer.
    checkYStep =
      let (u1,g3) = R.randomR (0,1) g2
          y = y_l + u1 * (y_l_plus1 - y_l)
      in
      case y < nonnormalized_pdf x of
        True -> (x,g3)
        False -> ziggurat table g3
      
    -- x did not fall outside the rectangle, and we are
    -- in the bottom layer, i.e. the tail
    fallbackToTailStep g0 =
      let (u0,g1) = R.randomR (0,1) g0
          (u1,g2) = R.randomR (0,1) g1
          x = (-(log u0)) / zigX0
          y = -(log u1)
      in
      case 2 * y > x * x of
        True -> (x + zigX0, g2)
        False -> fallbackToTailStep g2


histogram :: (Fl.Foldable f, RealFrac a)
          => Int    -- ^ num buckets, must be >= 3
          -> (a,a)  -- ^ range, (lo,hi)
          -> f a    -- ^ the samples
          -> [Int]
histogram nbuckets (lo,hi) samples = 
  fmap snd $ IM.toAscList $ Fl.foldl' tallySample initCounts samples
  where
    initCounts = IM.fromList $ take nbuckets $ zip [0,1..] (repeat 0)

    tallySample counts x =
      IM.adjust (\c -> c+1) k counts
      where
        k = sampleToBucket x

    sampleToBucket x =
      case x < lo of
        True -> 0
        False ->
          case x > hi of
            True -> nbuckets - 1
            False -> floor ((x - lo) / bucketWidth) + 1

    bucketWidth = intervalWidth / fromIntegral nbuckets'
    intervalWidth = hi - lo
    nbuckets' = nbuckets - 2

{- a type p in which we may produce random gaussian values.
 - a type v with which we represent variance or covariances
 - for a value in p
 -
 - minimum required definition: normal, normalD
 -}
class RandomGaussian p v | p -> v where
  normal :: (R.RandomGen g) => g -> (p, g)
  normalD :: (R.RandomGen g) => (p,v) -> g -> (p,g)

  normals :: (R.RandomGen g) => g -> [p]
  normals g = (\(x,g') -> x : normals g') (normal g)


  normalDs :: (R.RandomGen g) => (p,v) -> g -> [p]
  normalDs vd g = (\(x,g') -> x : normalDs vd g') (normalD vd g)

  normalDgs :: (R.RandomGen g) => (p,v) -> g -> [(p,g)]
  normalDgs vd g = (\(x,g') -> (x,g') : normalDgs vd g') (normalD vd g)


instance RandomGaussian Float Float where
  normal g = ziggurat zigTableFloats g

  normalD (m,v) g =
    (p', g')
    where
      p' = m + p * v
      (p,g') = ziggurat zigTableFloats g

-- identical to the float case
instance RandomGaussian Double Double where
  normal g = ziggurat zigTableDoubles g

  normalD (m,v) g =
    (p', g')
    where
      p' = m + p * v
      (p,g') = ziggurat zigTableDoubles g
