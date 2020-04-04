
import Data.Maybe
import System.Clock
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Control.Concurrent
import Control.Concurrent.MVar
import Text.Printf

import Params


getTics :: (Double,Double) -> [Double]
getTics (a,b) = [ a+i*(b-a)/10 | i <- [0 .. 10] ]

drawGrid :: Params -> Render ()
drawGrid φ = do
        let (w,h,width,height) = boxDimensions φ

        setLineWidth 1
        setSourceRGB 0.4 0.4 0.4

        -- Draw the box
        moveTo w h
        lineTo w (h+height)
        lineTo (w+width) (h+height)
        lineTo (w+width) h
        closePath
        stroke

        setSourceRGB 0.8 0.8 0.8
        let xtics = map (\fx -> (fst $ fnScrnSpc φ (fx,0), fx)) $ getTics $ φxrange φ
        mapM_ (\(xt, _) -> do
              moveTo xt h
              lineTo xt (h+height)
              ) xtics
        stroke

        let ytics = map (\fy -> (snd $ fnScrnSpc φ (0,fy), fy)) $ getTics $ φyrange φ
        mapM_ (\(yt, _) -> do
              moveTo w yt
              lineTo (w+width) yt
              ) ytics
        stroke

        let displayCoordinate = printf "%.1f" :: Double -> String

        setSourceRGB 0.4 0.4 0.4
        mapM_ (\(xt, fx) -> do
              let fxs = displayCoordinate fx

              TextExtents {textExtentsWidth = tw, textExtentsHeight = th} <- textExtents $ fxs
              moveTo (xt-tw/2) (h+height+th+5)
              showText fxs
              ) xtics
        mapM_ (\(yt, fy) -> do
              let fys = displayCoordinate fy

              TextExtents {textExtentsWidth = tw, textExtentsHeight = th} <- textExtents $ fys
              moveTo (w - tw - 7) (yt + th/2)
              showText fys
              ) ytics
        stroke

boxDimensions φ = (10+tb,10,winw-20-tb,winh-20-tb) where
    tb = 20
    winw = fromIntegral $ φw φ
    winh = fromIntegral $ φh φ

-- Converts clip space x∊[0,1] and y∊[0,1] to plot window space
clipScrnSpc φ (x,y) = (x*width + w, height-y*height + h) where
    (w,h,width,height) = boxDimensions φ

fnClipSpc φ (x,y) = ((x - x0)/(x1-x0), (y -y0)/(y1-y0)) where
        (x0,x1) = φxrange φ
        (y0,y1) = φyrange φ

fnScrnSpc φ = clipScrnSpc φ . fnClipSpc φ

drawGraphs :: Params -> Render ()
drawGraphs φ = do
        drawGrid φ

        -- Plot window
        let (w,h,width,height) = boxDimensions φ
        let (x0,x1) = φxrange φ
        let (y0,y1) = φyrange φ
        let samples = fromIntegral $ φsamples φ

        setLineWidth 1
        setSourceRGB 0.1 0.5 0.1

        let xs = [ x0 + abs (x1 - x0) * d / samples | d <- [0 .. samples] ]

        let fltrInf = filter (not . isInfinite . snd)
        let fltrClp = filter (\(x,y) -> y > h && y < h + height)

        let discretePts fn = case fn of
                    (Continuous f) -> map (\x->(x,f x)) xs
                    (Discrete pts) -> pts
        let lines = map (fnScrnSpc φ) . discretePts
        let drw f = do uncurry moveTo $ head  $ lines f
                       mapM_ (uncurry lineTo) (lines f)
        mapM_ drw $ φgraphs φ

        stroke

updCanvas c = do
        threadsEnter
        dw <- widgetGetWindow c

        drawWindowBeginPaintRect (fromJust dw) (Rectangle 0 0 100 100)
        drawWindowEndPaint (fromJust dw)

main :: IO ()
main = do

        let pulse x | x > 0 && x < 1 = 1
                    | otherwise = 0
        let pwrSeries n x = sum $ map (x**) [0..n]
        let fns = map Continuous $ (\x -> 1/(1-x)) : (pwrSeries <$> [1..15])

        let p = Params { φw = 600
                       , φh = 400
                       , φxView = View 0 10
                       , φyView = View 0 10
                       , φsamples = 100
                       , φgraphs = fns
                       }



        initGUI
        w <- windowNew
        set w [ windowTitle := "hnuplot", windowDefaultWidth := φw p, windowDefaultHeight := φh p ]

        f <- frameNew
        containerAdd w f
        c <- drawingAreaNew
        containerAdd f c
        widgetModifyBg c StateNormal (Color 65535 65535 65535)
        widgetShowAll w

        on w objectDestroy mainQuit


        drw <- on c draw (drawGraphs p)
        mainGUI

        putStrLn "threadGUI exit"

        putStrLn "threadLive entering"
        isCurrentThreadBound >>= print

        putStrLn "threadLive exit"

