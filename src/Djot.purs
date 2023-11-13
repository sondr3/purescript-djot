module Djot where

import Prelude

import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Maybe (Maybe(..), isJust)
import Data.String (toLower)
import Data.Tuple (Tuple)
import Data.Undefined.NoProblem (Opt, opt, toMaybe)
import Effect (Effect)
import Foreign.Object (Object)
import Foreign.Object as Object

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
  { attributes :: Opt Attributes
  , pos :: Opt Pos
  | a
  }

type HasChildren t a = { children :: Array t | a }

type HasText a = { text :: String | a }

type Para = HasAttributes (chilren :: Array Inline)
type Heading = HasAttributes (level :: Int, children :: Array Inline)
type ThematicBreak = HasAttributes ()
type Section = HasAttributes (children :: Array Block)
type Div = HasAttributes (children :: Array Block)
type BlockQuote = HasAttributes (children :: Array Block)
type CodeBlock = HasAttributes (lang :: Maybe String, text :: String)
type RawBlock = HasAttributes (format :: String, text :: String)

data BulletListStyle = BulletListPlus | BulletListMinus | BulletListStar
type BulletList = HasAttributes (tight :: Boolean, style :: BulletListStyle, children :: Array ListItem)
type ListItem = HasAttributes (children :: Array Block)

data CheckboxStatus = Checked | Unchecked
type TaskList = HasAttributes (tight :: Boolean, children :: Array TaskListItem)
type TaskListItem = HasAttributes (checkbox :: CheckboxStatus, children :: Array Block)

data OrderedListStyle
  = NumDot
  | NumParen
  | ParenNum
  | LowerADot
  | LowerAParen
  | ParenLowerA
  | UpperADot
  | UpperAParen
  | ParenUpperA
  | LowerIDot
  | LowerIParen
  | ParenLowerI
  | UpperIDot
  | UpperIParen
  | ParenUpperI

type OrderedList = HasAttributes
  ( style :: OrderedListStyle
  , tight :: Boolean
  , start :: Maybe Int
  , children :: Array ListItem
  )

type Caption = HasAttributes (children :: Array Inline)
type Table = HasAttributes (children :: Array TableRow)

data Block
  = ParaBlock Para
  | HeadingBlock Heading
  | ThematicBreakBlock ThematicBreak
  | SectionBlock Section
  | DivBlock Div
  | CodeBlockBlock CodeBlock
  | RawBlockBlock RawBlock
  | BlockQuoteBlock BlockQuote
  | OrderedListBlock OrderedList
  | BulletListBlock BulletList
  | TaskListBlock TaskList
  | DefinitionListBlock DefinitionList
  | TableBlock Table

type Str = HasAttributes (text :: String)
type FootnoteReference = HasAttributes (text :: String)

data SmartPunctuationType
  = LeftSingleQuote
  | RightSingleQuote
  | LeftDoubleQuote
  | RightDoubleQuote
  | Ellipses
  | EmDash
  | EnDash

type SmartPunctuation = HasAttributes (type :: SmartPunctuationType, text :: String)

type SoftBreak = HasAttributes ()
type HardBreak = HasAttributes ()
type NonBreakingSpace = HasAttributes ()
type Symb = HasAttributes (alias :: String)
type Verbatim = HasAttributes (text :: String)
type RawInline = HasAttributes (format :: String, text :: String)
type InlineMath = HasAttributes (text :: String)
type DisplayMath = HasAttributes (text :: String)
type Url = HasAttributes (text :: String)
type Email = HasAttributes (text :: String)
type Link = HasAttributes (destination :: Maybe String, reference :: Maybe String, children :: Array Inline)
type Image = HasAttributes (destination :: Maybe String, reference :: Maybe String, children :: Array Inline)
type Emph = HasAttributes (children :: Array Inline)
type Strong = HasAttributes (children :: Array Inline)
type Span = HasAttributes (children :: Array Inline)
type Mark = HasAttributes (children :: Array Inline)
type Superscript = HasAttributes (children :: Array Inline)
type Subscript = HasAttributes (children :: Array Inline)
type Delete = HasAttributes (children :: Array Inline)
type Insert = HasAttributes (children :: Array Inline)
type DoubleQuoted = HasAttributes (children :: Array Inline)
type SingleQuoted = HasAttributes (children :: Array Inline)

type DefinitionList = HasAttributes (children :: Array DefinitionListItem)
type DefinitionListItem = HasAttributes (children :: Array (Tuple Term Definition))

type Term = HasAttributes (children :: Array Inline)
type Definition = HasAttributes (children :: Array Block)

data CellAlignment = AlignLeft | AlignRight | AlignCenter | AlignDefault
type TableRow = HasAttributes (head :: Boolean, children :: Array Cell)
type Cell = HasAttributes (head :: Boolean, align :: Maybe CellAlignment, children :: Array Inline)

type Reference = HasAttributes (label :: String, destination :: String)
type Footnote = HasAttributes (label :: String, children :: Array Block)

data Inline
  = StrInline Str
  | SoftBreakInline SoftBreak
  | HardBreakInline HardBreak
  | NonBreakingSpaceInline NonBreakingSpace
  | SymbInline Symb
  | VerbatimInline Verbatim
  | RawInlineInline RawInline
  | InlineMathInline InlineMath
  | DisplayMathInline DisplayMath
  | UrlInline Url
  | EmailInline Email
  | FootnoteReferenceInline FootnoteReference
  | SmartPunctuationInline SmartPunctuation
  | EmphInline Emph
  | StrongInline Strong
  | LinkInline Link
  | ImageInline Image
  | SpanInline Span
  | MarkInline Mark
  | SuperscriptInline Superscript
  | SubscriptInline Subscript
  | InsertInline Insert
  | DeleteInline Delete
  | DoubleQuotedInline DoubleQuoted
  | SingleQuotedInline SingleQuoted

type Doc = HasAttributes
  ( references :: Object Reference
  , footnotes :: Object Footnote
  , children :: Array Block
  )

type Visitor a b =
  { doc :: Maybe (Doc -> a -> b)
  , para :: Maybe (Para -> a -> b)
  , heading :: Maybe (Heading -> a -> b)
  , thematic_break :: Maybe (ThematicBreak -> a -> b)
  , section :: Maybe (Section -> a -> b)
  , div :: Maybe (Div -> a -> b)
  , code_block :: Maybe (CodeBlock -> a -> b)
  , raw_block :: Maybe (RawBlock -> a -> b)
  , block_quote :: Maybe (BlockQuote -> a -> b)
  , ordered_list :: Maybe (OrderedList -> a -> b)
  , bullet_list :: Maybe (BulletList -> a -> b)
  , task_list :: Maybe (TaskList -> a -> b)
  , definition_list :: Maybe (DefinitionList -> a -> b)
  , table :: Maybe (Table -> a -> b)
  , str :: Maybe (Str -> a -> b)
  , soft_break :: Maybe (SoftBreak -> a -> b)
  , hard_break :: Maybe (HardBreak -> a -> b)
  , non_breaking_space :: Maybe (NonBreakingSpace -> a -> b)
  , symb :: Maybe (Symb -> a -> b)
  , verbatim :: Maybe (Verbatim -> a -> b)
  , raw_inline :: Maybe (RawInline -> a -> b)
  , inline_math :: Maybe (InlineMath -> a -> b)
  , display_math :: Maybe (DisplayMath -> a -> b)
  , url :: Maybe (Url -> a -> b)
  , email :: Maybe (Email -> a -> b)
  , footnote_reference :: Maybe (FootnoteReference -> a -> b)
  , smart_punctuation :: Maybe (SmartPunctuation -> a -> b)
  , emph :: Maybe (Emph -> a -> b)
  , strong :: Maybe (Strong -> a -> b)
  , link :: Maybe (Link -> a -> b)
  , image :: Maybe (Image -> a -> b)
  , span :: Maybe (Span -> a -> b)
  , mark :: Maybe (Mark -> a -> b)
  , superscript :: Maybe (Superscript -> a -> b)
  , subscript :: Maybe (Subscript -> a -> b)
  , insert :: Maybe (Insert -> a -> b)
  , delete :: Maybe (Delete -> a -> b)
  , double_quoted :: Maybe (DoubleQuoted -> a -> b)
  , single_quoted :: Maybe (SingleQuoted -> a -> b)
  , list_item :: Maybe (ListItem -> a -> b)
  , task_list_item :: Maybe (TaskListItem -> a -> b)
  , definition_list_item :: Maybe (DefinitionListItem -> a -> b)
  , term :: Maybe (Term -> a -> b)
  , definition :: Maybe (Definition -> a -> b)
  , row :: Maybe (TableRow -> a -> b)
  , cell :: Maybe (Cell -> a -> b)
  , caption :: Maybe (Caption -> a -> b)
  , footnote :: Maybe (Footnote -> a -> b)
  , reference :: Maybe (Reference -> a -> b)
  }

type HTMLVisitor = Visitor HTMLRenderer String

emptyVisitor :: HTMLVisitor
emptyVisitor =
  { doc: Nothing
  , para: Nothing
  , heading: Nothing
  , thematic_break: Nothing
  , section: Nothing
  , div: Nothing
  , code_block: Nothing
  , raw_block: Nothing
  , block_quote: Nothing
  , ordered_list: Nothing
  , bullet_list: Nothing
  , task_list: Nothing
  , definition_list: Nothing
  , table: Nothing
  , str: Nothing
  , soft_break: Nothing
  , hard_break: Nothing
  , non_breaking_space: Nothing
  , symb: Nothing
  , verbatim: Nothing
  , raw_inline: Nothing
  , inline_math: Nothing
  , display_math: Nothing
  , url: Nothing
  , email: Nothing
  , footnote_reference: Nothing
  , smart_punctuation: Nothing
  , emph: Nothing
  , strong: Nothing
  , link: Nothing
  , image: Nothing
  , span: Nothing
  , mark: Nothing
  , superscript: Nothing
  , subscript: Nothing
  , insert: Nothing
  , delete: Nothing
  , double_quoted: Nothing
  , single_quoted: Nothing
  , list_item: Nothing
  , task_list_item: Nothing
  , definition_list_item: Nothing
  , term: Nothing
  , definition: Nothing
  , row: Nothing
  , cell: Nothing
  , caption: Nothing
  , footnote: Nothing
  , reference: Nothing
  }

emphVisitor :: HTMLVisitor -> HTMLVisitor
emphVisitor v = buildOverrides $ v
  { emph = Just emphRenderer
  , section = Just sectionRenderer
  , heading = Just headingRenderer
  }

defaultRenderer :: RenderHTMLOptions
defaultRenderer = RenderHTMLOptions
  { overrides: Just $ emphVisitor emptyVisitor
  , warn: Nothing
  }

emphRenderer :: Emph -> HTMLRenderer -> String
emphRenderer node ctx = "<span class='emphasis'>" <> (ctx.renderChildren node) <> "</span>"

lookupAttribute :: String -> Attributes -> Maybe String
lookupAttribute key attrs = Object.lookup key attrs

getAttributes :: forall a. HasAttributes a -> Attributes
getAttributes node = case toMaybe node.attributes of
  Just attrs -> attrs
  Nothing -> Object.empty

hasAttribute ∷ ∀ a. HasAttributes a → String → Boolean
hasAttribute node key = isJust $ toMaybe node.attributes >>= lookupAttribute key

maybeAttribute ∷ ∀ a. HasAttributes a → String → Maybe String
maybeAttribute node key = toMaybe node.attributes >>= lookupAttribute key

insertAttribute :: forall a. HasAttributes a -> String -> (String -> String) -> HasAttributes a
insertAttribute node key f = case maybeAttribute node key of
  Just val -> node { attributes = opt $ Object.insert key (f val) (getAttributes node) }
  Nothing -> node

sectionRenderer :: Section -> HTMLRenderer -> String
sectionRenderer node ctx = ctx.renderAstNodeDefault $ insertAttribute node "id" (toLower)

headingRenderer :: Heading -> HTMLRenderer -> String
headingRenderer node ctx = ctx.renderAstNodeDefault node

type HTMLRenderer =
  { renderChildren :: forall t a. HasChildren t a -> String
  , renderAstNodeDefault :: forall a. HasAttributes a -> String
  }

foreign import renderWarning_ :: Warning -> String

foreign import buildOverridesImpl :: forall a. Fn2 (Maybe a -> Boolean) (Visitor HTMLRenderer String) (Visitor HTMLRenderer String)

buildOverrides :: Visitor HTMLRenderer String -> Visitor HTMLRenderer String
buildOverrides = runFn2 buildOverridesImpl isJust

foreign import renderHtmlImpl :: forall a. Fn3 (Maybe a -> Boolean) Doc (Maybe RenderHTMLOptions) String

renderHtml :: Doc -> (Maybe RenderHTMLOptions) -> String
renderHtml = runFn3 renderHtmlImpl isJust

foreign import parseImpl :: Fn2 String (Maybe ParseOptions) Doc

parse :: String -> (Maybe ParseOptions) -> Doc
parse input = runFn2 parseImpl input