module StreamInput (mainLoopStream) where

import System.IO
import System.IO.Error (isEOFError)
import Data.Time.Clock
import Control.Exception (catch)

consume :: IO a -> IO (Maybe a)
consume f = (Just <$> f) `catch` \e -> if isEOFError e then return Nothing else ioError e

stampInput :: IO (UTCTime, String)
stampInput = do
        ln <- hGetLine stdin
        t <- getCurrentTime
        return (t, ln)

readLoop :: UTCTime -> ([[(Double,Double)]] -> IO ()) -> [(UTCTime, String)] -> IO [(UTCTime, String)]
readLoop t0 upd vs = do
        inp <- consume $ stampInput

        case inp of
            Nothing -> return vs
            Just v -> do
                let cutoff = 15 :: NominalDiffTime

                let vs' = dropWhile ((> cutoff) . diffUTCTime (fst v) . fst) $ vs ++ [v]
                let discreteFns = map (\(t, s) -> (convT t, map (read :: String -> Double) $ words s)) where
                    convT = fromRational . toRational . diffUTCTime t0

                let separate dfns = let l = length $ snd $ head dfns in
                                            map (\i -> map ( \(t, vals) -> (t, vals !! i) ) dfns) [0..l-1]

                upd $ separate $ discreteFns vs'
                readLoop t0 upd vs'

mainLoopStream :: ([[(Double,Double)]] -> IO ()) -> IO ()
mainLoopStream upd = do
        t0 <- getCurrentTime
        i <- readLoop t0 upd []
        print i
