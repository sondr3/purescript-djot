module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Djot (defaultRenderer, parse, renderHtml)
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  let parsed = parse "# About me\n_hi_ there\nfriend\n\nnew para" Nothing
  let html = renderHtml parsed $ Just defaultRenderer
  log html
