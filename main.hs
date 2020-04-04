
import StreamInput (mainLoopStream)
import Data.Maybe
import System.Clock
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Control.Concurrent
import Control.Concurrent.MVar
import Graphics.UI.Gtk.Gdk.EventM
import Data.Text (unpack)
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

drawDecorations φ = do
        -- Paint background
        setSourceRGB 1.0 1.0 1.0
        paint

        drawGrid φ

drawGraphs :: Params -> Render ()
drawGraphs φ = do
        drawDecorations φ
        -- Plot window
        let (w,h,width,height) = boxDimensions φ
        let (x0,x1) = φxrange φ
        let (y0,y1) = φyrange φ
        let samples = fromIntegral $ φsamples φ

        setLineWidth 1
        let colours = [ setSourceRGB 0.1 0.5 0.1
                      , setSourceRGB 0.1 0.1 0.5
                      , setSourceRGB 0.5 0.1 0.1
                      ]

        let xs = [ x0 + abs (x1 - x0) * d / samples | d <- [0 .. samples] ]

        let fltrInf = filter (not . isInfinite . snd)

        let discretePts fn = case fn of
                    (Continuous f) -> map (\x->(x,f x)) xs
                    (Discrete pts) -> pts
        let lines = map (fnScrnSpc φ) . discretePts

        let drw (colourIndex, f) = do
                       colours !! colourIndex

                       let lns = lines f
                       if length lns >= 1 then uncurry moveTo $ head $ lns
                                          else return ()
                       mapM_ (uncurry lineTo) lns
                       stroke

        mapM_ drw $ zip (cycle [ 0 .. (length colours) - 1]) $ φgraphs φ

update φv ptss = putMVar φv $ \φ -> φ { φgraphs = map Discrete ptss }

mainUpdateφv c φv φ drwSigMV var = do
        postGUIAsync $ do
            drw <- on c draw (drawGraphs φ)

            oldDrw <- tryTakeMVar drwSigMV
            case oldDrw of
                Nothing -> return ()
                Just d -> signalDisconnect d

            putMVar drwSigMV drw

            widgetQueueDraw c

        φλ <- takeMVar φv

        let φ' = φλ φ
        mainUpdateφv c φv φ' drwSigMV (var + 1)


mainScrollX φv mt = do
        t <- getCurrentTime
        let t0 = case mt of
                 Nothing -> t
                 Just t0 -> t0

        let δ = fromRational $ toRational $ diffUTCTime t0 t

        putMVar φv (\φ -> if (φmode φ) == Follow
                                then φ {φxView = (φxView φ) { offset = δ }}
                                else φ
                   )

        threadDelay $ 5 * 10^3
        mainScrollX φv $ Just t0


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
                       , φmode = Follow
                       }

        φv <- newMVar (\_ -> p)
        initGUI
        w <- windowNew
        set w [ windowTitle := "hnuplot", windowDefaultWidth := φw p, windowDefaultHeight := φh p ]

        f <- frameNew
        containerAdd w f
        c <- drawingAreaNew
        containerAdd f c
        widgetShowAll w

        on w objectDestroy mainQuit

        drwSigMV <- newEmptyMVar
        forkIO $ mainUpdateφv c φv p drwSigMV 0
        forkIO $ mainLoopStream (update φv)
        forkIO $ mainScrollX φv Nothing

        on w keyReleaseEvent $ tryEvent $ do
              "q" <- fmap unpack eventKeyName
              liftIO $ widgetDestroy w

        on w keyReleaseEvent $ tryEvent $ do
              "f" <- fmap unpack eventKeyName
              liftIO $ putMVar φv $ \φ -> φ { φmode = toggleMode $ φmode φ }

        on w scrollEvent $ tryEvent $ do
              mods <- eventModifier
              direction <- eventScrollDirection
              let multiplier = case direction of
                               ScrollUp -> 1
                               ScrollDown -> -1
                               otherwise -> 0

              let modView = if Control `elem` mods
                                then \view -> view { Params.width = (Params.width view) + multiplier }
                                else \view -> view { offset = (offset view) - multiplier }

              let modφ = if Shift `elem` mods
                              then \φ -> φ { φxView = modView $ φxView φ }
                              else \φ -> φ { φyView = modView $ φyView φ }

              if multiplier /= 0
                  then liftIO $ putMVar φv modφ
                  else return ()

        mainGUI

        putStrLn "threadGUI exit"

        putStrLn "threadLive entering"
        isCurrentThreadBound >>= print

        putStrLn "threadLive exit"

