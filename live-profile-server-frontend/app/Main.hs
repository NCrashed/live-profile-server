{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Diagrams.Backend.Reflex as DR
import Diagrams.Prelude as D
import Reflex as R
import Reflex.Dom as R
import Text.Printf

import qualified Data.Vector as V

import Profile.Live.Server.Client.Bined

main :: IO ()
main = mainWidget binnedDiagramDebugWidget

