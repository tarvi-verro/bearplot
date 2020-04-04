module Params where

data Function = Continuous (Double -> Double)
              | Discrete [(Double, Double)]

instance Show Function where
        show f = case f of
                     (Continuous _) -> "Continuous fn"
                     (Discrete _) -> "Discrete fn"

data View = View { offset :: Double
                 , width :: Double
                 } deriving Show

viewToRange (View o w)  = (o, o + w)
φxrange Params { φxView = v } = viewToRange v
φyrange Params { φyView = v } = viewToRange v

data Mode = Follow | Fixed
          deriving (Show, Eq, Enum)

toggleMode Follow = Fixed
toggleMode Fixed = Follow

data Params = Params { φw :: Int
                     , φh :: Int
                     , φxView :: View
                     , φyView :: View
                     , φsamples :: Int
                     , φgraphs :: [Function]
                     , φmode :: Mode
            } deriving (Show)

