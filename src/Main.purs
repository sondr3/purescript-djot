module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Djot (ParseOptions(..), RenderHTMLOptions(..), defaultRenderer, parse, renderHtml)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)

parseOpts :: ParseOptions
parseOpts = ParseOptions
  { sourcePositions: true
  , warn: Just (\warn -> logShow warn)
  }

renderOpts :: RenderHTMLOptions
renderOpts = RenderHTMLOptions
  { overrides: Nothing
  , warn: Just (\warn -> logShow warn)
  }

main :: Effect Unit
main = do
  let parsed = parse "# About me\n_hi_ there\nfriend\n\nnew para" $ Just parseOpts
  let html = renderHtml parsed $ Just defaultRenderer
  log html
