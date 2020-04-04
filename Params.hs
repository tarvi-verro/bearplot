module Params where

data Params = Params { φw :: Int
                     , φh :: Int
                     , φxrange :: (Double,Double)
                     , φyrange :: (Double,Double)
                     , φsamples :: Int
            } deriving (Show)

