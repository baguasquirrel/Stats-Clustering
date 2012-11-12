module Statistics.Gaussian (pdf, std_pdf, nonnormalized_pdf, nonnormalized_inverse_pdf, erfc, zigTable) where

import qualified Data.Vector as V

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


nonnormalized_inverse_pdf :: (Floating a) => a -> a
nonnormalized_inverse_pdf x = (sqrt (pi * 0.5)) * (erfc (x * 0.7071067811865475))


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
  (V.fromList . take zigTableSize) zigList
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
