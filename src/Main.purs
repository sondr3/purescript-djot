module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Djot (Doc, ParseOptions(..), RenderOptions(..), parse_, renderHtml_)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)

parseOpts :: ParseOptions
parseOpts = ParseOptions
  { sourcePositions: true
  , warn: Just (\warn -> logShow warn)
  }

renderOpts :: RenderOptions
renderOpts = RenderOptions
  { wrapWidth: Nothing
  , warn: Just (\warn -> logShow warn)
  }

parse :: String -> Maybe ParseOptions -> Doc
parse input opts = parse_ input opts

renderHtml :: Doc -> Maybe RenderOptions -> String
renderHtml doc opts = renderHtml_ doc opts

main :: Effect Unit
main = do
  let parsed = parse "hi there\nfriend\n\nnew para" $ Just parseOpts
  -- log $ show parsed
  let html = renderHtml parsed $ Just renderOpts
  log html
