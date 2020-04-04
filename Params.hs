module Params where

data View = View { offset :: Double
                 , width :: Double
                 } deriving Show

viewToRange (View o w)  = (o, o + w)
φxrange Params { φxView = v } = viewToRange v
φyrange Params { φyView = v } = viewToRange v

data Params = Params { φw :: Int
                     , φh :: Int
                     , φxView :: View
                     , φyView :: View
                     , φsamples :: Int
            } deriving (Show)

