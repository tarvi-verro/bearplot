import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.Concurrent (threadDelay)
import System.IO (stdout, hFlush)

printT t0 = do
        threadDelay $ 5 * 10^4
        t <- getCurrentTime
        let δ = fromRational $ toRational $ diffUTCTime t t0
        putStrLn $ unwords $ map show [ ((+3.1) . (*3)) $ sin $ δ * 3,  ((+3.1) . (*3)) $ sin $ (δ + 3.14) * 3]
        hFlush stdout
        printT t0

main :: IO ()
main = do
        t <- getCurrentTime
        printT t
