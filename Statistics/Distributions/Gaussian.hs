{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Statistics.Distributions.Gaussian
  ( pdf
  , std_pdf
  , nonnormalized_pdf
  , nonnormalized_inverse_pdf
  , erfc
  , zigTable
  , zigTableFloats, wordToFloating
  , ziggurat
  , UniformGaussianValues(..)
  , histogram

  ) where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified System.Random as R
import qualified Data.Foldable as Fl
import qualified Data.IntMap as IM
import Data.Word
import Data.Bits

import System.IO.Unsafe


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

{-# INLINABLE nonnormalized_pdf #-}
nonnormalized_pdf :: (Floating a)
                  => a
                  -> a
nonnormalized_pdf x = exp (-0.5 * x * x)

area_of_tail_pdf :: (Floating a) => a -> a
area_of_tail_pdf x = (sqrt (pi * 0.5)) * (erfc (x * 0.7071067811865475))

{-# INLINABLE nonnormalized_inverse_pdf #-}
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
{-# INLINE zigTableSize #-}
zigTableSize = 0x80
{-# INLINE zigTableMask #-}
zigTableMask = 0x7F

{-# INLINE gaussian_zigX1 #-}
gaussian_zigX1 :: (Floating a) => a
gaussian_zigX1 = 3.442619855899

{-# INLINE gaussian_zigA #-}
gaussian_zigA :: (Floating a) => a
gaussian_zigA = 9.91256303526217e-3

{-# INLINE gaussian_zigADivY0 #-}
gaussian_zigADivY0 :: (Floating a) => a
gaussian_zigADivY0 = gaussian_zigA / nonnormalized_pdf gaussian_zigX1

zigTable :: (RealFrac a, Floating a)
         => a
         -> a
         -> ((a -> a), (a -> a))
         -> (V.Vector a, V.Vector a, V.Vector Word)
{-# SPECIALISE INLINE zigTable :: Float -> Float -> ((Float -> Float), (Float -> Float)) -> (V.Vector Float, V.Vector Float, V.Vector Word) #-}
{-# SPECIALISE INLINE zigTable :: Double -> Double -> ((Double -> Double), (Double -> Double)) -> (V.Vector Double, V.Vector Double, V.Vector Word) #-}
zigTable zigX1 zigA (pdf, invpdf) =
  (V.fromList xs, V.fromList ys, V.fromList $ xw0 : g (tail zigList))
  where
    (xs, ys) = unzip zigList
    zigList = (take zigTableSize zigList') ++ [(0, pdf 0)]
    zigList' = (x0, y0) : (x1, y1) : f (x1, y1)

    x0 = zigX1
    y0 = pdf zigX1
    xw0 = round $ ((zigX1 * y0) / zigA) * (fromIntegral (maxBound :: Word))

    x1 = zigX1
    y1 = y0 + zigA / x1

    g ((x_i,y_i) : (x_ip1,y_ip1) : zs) = (round $ (x_ip1 / x_i) * (fromIntegral (maxBound :: Word))) : (g ((x_ip1,y_ip1) : zs))
    g _ = []

    f (x_p, y_p) =
      p_c : (f p_c)
      where
        p_c = (x_c, y_c)
        x_c = invpdf y_p
        y_c = y_p + zigA / x_c

{-# SPECIALISE zigTableGaussian :: (V.Vector Float, V.Vector Float, V.Vector Word) #-}
{-# SPECIALISE zigTableGaussian :: (V.Vector Double, V.Vector Double, V.Vector Word) #-}
zigTableGaussian :: (RealFrac p, Floating p) => (V.Vector p, V.Vector p, V.Vector Word)
zigTableGaussian = zigTable gaussian_zigX1 gaussian_zigA (nonnormalized_pdf, nonnormalized_inverse_pdf)

zigTableFloats :: (V.Vector Float, V.Vector Float, V.Vector Word)
zigTableFloats = zigTable gaussian_zigX1 gaussian_zigA (nonnormalized_pdf, nonnormalized_inverse_pdf)

zigTableDoubles :: (V.Vector Double, V.Vector Double, V.Vector Word)
zigTableDoubles = zigTable gaussian_zigX1 gaussian_zigA (nonnormalized_pdf, nonnormalized_inverse_pdf)

{-# SPECIALISE INLINE wordToFloating :: Word -> Float #-}
{-# SPECIALISE INLINE wordToFloating :: Word -> Double #-}
wordToFloating :: (Floating p) => Word -> p
wordToFloating w = (fromIntegral w) * wordToFloatingInv

{-# SPECIALISE INLINE wordToFloatingInv :: Float #-}
{-# SPECIALISE INLINE wordToFloatingInv :: Double #-}
wordToFloatingInv :: (Floating p) => p
wordToFloatingInv = 1.0 / (fromIntegral (maxBound :: Word))


{-# SPECIALISE INLINE ziggurat :: (R.RandomGen g) => (V.Vector Float, V.Vector Float, V.Vector Word) -> g -> (Float,g) #-}
{-# SPECIALISE INLINE ziggurat :: (R.RandomGen g) => (V.Vector Double, V.Vector Double, V.Vector Word) -> g -> (Double,g) #-}
{-# INLINE ziggurat #-}
ziggurat :: (Floating p, Ord p, R.Random p, Show p, R.RandomGen g)
         => (V.Vector p, V.Vector p, V.Vector Word)
         -> g
         -> (p,g)
ziggurat (tableX, tableY, tableXW) !g0 =
  checkBottomLayer
  where
    -- choose random layer
    ((!u1) :: Word, !g1) = R.random g0
    !l = fromIntegral $ u1 .&. zigTableMask
    !sign = 
      case (u1 .&. 0x80) == 0 of
        True ->  -1.0
        False ->  1.0

    ((!u2) :: Word, !g2) = R.random g1

    {-# INLINE checkBottomLayer #-}
    checkBottomLayer =
      case l == 0 of
        False -> checkXStep
        True ->
          case u2 < tableXW `V.unsafeIndex` 0 of
            False -> fallbackToTail g2
            True ->
              (x',g2)
              where
                !x' = (wordToFloating u2) * gaussian_zigADivY0 * sign


    x = (wordToFloating u2) * (tableX `V.unsafeIndex` l)

    {-# INLINE checkXStep #-}
    checkXStep =
      case u2 < tableXW `V.unsafeIndex` l of
        True -> 
          (x', g2)  where !x' = x * sign

        False ->
          checkYStep

    fallbackToTail !gA =
      let x = -(log u0) / gaussian_zigX1
          y = -(log u1)
          (u0,!gB) = R.random gA
          (u1,!gC) = R.random gB
      in
      case y + y > x * x of
        True -> (x', gC)
          where
            !x' = sign * (x + gaussian_zigX1)

        False -> fallbackToTail gC


    {-# INLINE checkYStep #-}
    checkYStep =
      let (!d3,!g3) = R.random g2
          !y_lm1 = tableY `V.unsafeIndex` (l-1)
          !y_l =   tableY `V.unsafeIndex` l
      in
      case y_lm1 + ((y_l - y_lm1) * d3) < nonnormalized_pdf x of
        True -> (x', g3)  where !x' = x * sign
        False -> ziggurat (tableX, tableY, tableXW) g3




histogram :: (Fl.Foldable f, RealFrac a)
          => Int    -- ^ num buckets, must be >= 3
          -> (a,a)  -- ^ range, (lo,hi)
          -> f a    -- ^ the samples
          -> [Int]
histogram nbuckets (lo,hi) samples = 
  fmap snd $ IM.toAscList $ Fl.foldl' tallySample initCounts samples
  where
    initCounts = IM.fromList $ take nbuckets $ zip [0,1..] (repeat 0)

    tallySample !counts !x =
      IM.adjust (\c -> c+1) k counts
      where
        k = sampleToBucket x

    sampleToBucket !x =
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
 - a type v with which we represent variance for a random
 - variable in p
 -
 - This class is meant to represent uniform gaussian values,
 - which is to say that if we have an n-dimensional value,
 - each of the n variables is independent of each other.
 -
 - minimum required definition: normal, normalD
 -}
class UniformGaussianValues p v | p -> v where
  normal :: (R.RandomGen g) => g -> (p, g)
  normalD :: (R.RandomGen g) => (p,v) -> g -> (p,g)

  normals :: (R.RandomGen g) => g -> [p]
  normals g = (\(x,g') -> x : normals g') (normal g)


  normalDs :: (R.RandomGen g) => (p,v) -> g -> [p]
  normalDs vd g = (\(x,g') -> x : normalDs vd g') (normalD vd g)

  normalDgs :: (R.RandomGen g) => (p,v) -> g -> [(p,g)]
  normalDgs vd g = (\(x,g') -> (x,g') : normalDgs vd g') (normalD vd g)


instance UniformGaussianValues Float Float where
  normal g = ziggurat zigTableGaussian g

  normalD (m,v) g =
    (p', g')
    where
      p' = m + p * v
      (p,g') = ziggurat zigTableGaussian g

-- identical to the float case
instance UniformGaussianValues Double Double where
  normal g = ziggurat zigTableGaussian g

  normalD (m,v) g =
    (p', g')
    where
      p' = m + p * v
      (p,g') = ziggurat zigTableGaussian g
