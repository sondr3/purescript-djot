module Djot
  ( Attributes
  , Block(..)
  , Doc
  , Emph'
  , HTMLRenderer
  , HasAttributes
  , HasChildren
  , HasText
  , Heading'
  , InlineType(..)
  , Para'
  , ParseOptions(..)
  , Pos(..)
  , RenderHTMLOptions(..)
  , SourceLoc(..)
  , Visitor
  , Warning(..)
  , defaultRenderer
  , emphRenderer
  , emphVisitor
  , emptyVisitor
  , renderHtml_
  , renderWarning_
  , parse_
  , buildOverrides
  , renderHtml
  ) where

import Prelude

import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Maybe (Maybe(..), isJust)
import Effect (Effect)
import Foreign.Object (Object)

newtype SourceLoc = SourceLoc
  { line :: Int
  , col :: Int
  , offset :: Int
  }

instance show :: Show SourceLoc where
  show (SourceLoc { line: line, col: col }) = "{ " <> "line=" <> show line <> ", column=" <> show col <> " }"

newtype Pos = Pos
  { start :: SourceLoc
  , end :: SourceLoc
  }

data Warning = Warning
  { message :: String
  , offset :: Maybe Int
  , sourceLoc :: Maybe SourceLoc
  }

instance showWarning :: Show Warning where
  show warn = renderWarning_ warn

data RenderHTMLOptions = RenderHTMLOptions
  { overrides :: Maybe (Visitor HTMLRenderer String)
  , warn :: Maybe (Warning -> Effect Unit)
  }

data ParseOptions = ParseOptions
  { sourcePositions :: Boolean
  , warn :: Maybe (Warning -> Effect Unit)
  }

type Attributes = Object String

type HasAttributes a =
  { attributes :: Maybe Attributes
  , pos :: Maybe Pos
  | a
  }

type HasChildren a = { children :: Array InlineType | a }

type HasText a = { text :: String | a }

data InlineType = Emph Emph'

type Emph' = HasAttributes (children :: Array InlineType)

type Para' = HasAttributes (chilren :: Array InlineType)
type Heading' = HasAttributes (level :: Int, children :: Array InlineType)

data Block
  = Para Para'
  | Heading Heading'

type Visitor a b =
  { doc :: Maybe (Doc -> a -> b)
  , para :: Maybe (Para' -> a -> b)
  , emph :: Maybe (Emph' -> a -> b)
  }

emptyVisitor :: Visitor HTMLRenderer String
emptyVisitor = { doc: Nothing, para: Nothing, emph: Nothing }

emphVisitor :: Visitor HTMLRenderer String
emphVisitor = buildOverrides
  { doc: Nothing
  , para: Nothing
  , emph: Just (emphRenderer)
  }

defaultRenderer :: RenderHTMLOptions
defaultRenderer = RenderHTMLOptions
  { overrides: Just emphVisitor
  , warn: Nothing
  }

emphRenderer :: Emph' -> HTMLRenderer -> String
emphRenderer node ctx = "<span class='emphasis'>" <> ctx.renderChildren node <> "</span>"

foreign import data Doc :: Type

type HTMLRenderer =
  { renderChildren :: forall a. HasChildren a -> String
  }

foreign import parse_ :: String -> (Maybe ParseOptions) -> Doc
foreign import renderHtml_ :: Doc -> (Maybe RenderHTMLOptions) -> String
foreign import renderWarning_ :: Warning -> String

foreign import buildOverridesImpl :: forall a. Fn2 (Maybe a -> Boolean) (Visitor HTMLRenderer String) (Visitor HTMLRenderer String)

buildOverrides :: Visitor HTMLRenderer String -> Visitor HTMLRenderer String
buildOverrides = runFn2 buildOverridesImpl isJust

foreign import renderHtmlImpl :: forall a. Fn3 (Maybe a -> Boolean) Doc (Maybe RenderHTMLOptions) String

renderHtml :: Doc -> (Maybe RenderHTMLOptions) -> String
renderHtml = runFn3 renderHtmlImpl isJust