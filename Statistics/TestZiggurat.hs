import System.Environment
import System.IO
import Statistics.Distributions.Gaussian

-- from the package mersenne-random-pure64
import System.Random.Mersenne.Pure64

main :: IO ()
main =
  do n <- fmap (read . head) getArgs
     mt <- newPureMT
     let l = (take n $ normals mt) :: [Float]
         -- h = histogram 20 (-3.0,3.0) l

     -- print h
     devnull <- openFile "/dev/null" WriteMode
     mapM_ (hPutStr devnull . show) l