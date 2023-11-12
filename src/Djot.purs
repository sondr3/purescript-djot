module Djot
  ( Child(..)
  , Doc(..)
  , ParseOptions(..)
  , RenderOptions(..)
  , SourceLoc(..)
  , Warning(..)
  , parse_
  , renderHtml_
  , renderWarning_
  )
  where

import Data.Tuple (Tuple)
import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Effect (Effect)

newtype SourceLoc = SourceLoc {
  line :: Int,
  col :: Int,
  offset :: Int
}

instance show :: Show SourceLoc where
  show (SourceLoc { line: line, col: col }) = "{ " <> "line=" <> show line <> ", column=" <> show col <> " }"

data Warning = Warning {
  message :: String,
  offset :: Maybe Int,
  sourceLoc :: Maybe SourceLoc
}

instance showWarning :: Show Warning where 
  show warn = renderWarning_ warn

data RenderOptions = RenderOptions {
  wrapWidth :: Maybe Int,
  warn :: Maybe (Warning -> Effect Unit)
}

data ParseOptions = ParseOptions {
  sourcePositions :: Boolean,
  warn :: Maybe (Warning -> Effect Unit)
}

newtype Doc = Doc {
  tag :: String,
  references :: Array (Tuple String String), 
  footnotes :: Array (Tuple String String),
  children :: Array Child
}

newtype Child = Child {
  tag :: String,
  children :: Array Child,
  attributes :: Maybe (Array (Tuple String String)),
  pos :: Maybe SourceLoc
}

derive instance genericChild :: Generic Child _

instance showChild :: Show Child where
  show child = genericShow child

derive instance genericDoc :: Generic Doc _

instance showDoc :: Show Doc where
  show = genericShow

foreign import renderHtml_ :: Doc -> (Maybe RenderOptions) -> String
foreign import parse_ :: String -> (Maybe ParseOptions) -> Doc
foreign import renderWarning_ :: Warning -> String