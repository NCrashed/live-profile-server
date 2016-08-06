module Main where

import Graphics.Shine
import Graphics.Shine.Picture
import Graphics.Shine.Render
import GHCJS.DOM

concentricCircles :: Picture
concentricCircles = Translate 100 100 $ foldMap CircleF [1, 10..100]

main :: IO ()
main = runWebGUI $ \ webView -> do
    ctx <- fixedSizeCanvas webView 1920 900
    render ctx concentricCircles