module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Djot (Doc, ParseOptions(..), RenderHTMLOptions(..), buildOverrides, defaultRenderer, emphVisitor, parse_, renderHtml)
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

parse :: String -> Maybe ParseOptions -> Doc
parse input opts = parse_ input opts

renderHtml' :: Doc -> Maybe RenderHTMLOptions -> String
renderHtml' doc opts = renderHtml doc opts

main :: Effect Unit
main = do
  let parsed = parse "_hi_ there\nfriend\n\nnew para" $ Just parseOpts
  let html = renderHtml parsed $ Just defaultRenderer
  log html
