module Test.Main where

import Djot (Emph, HTMLRenderer, HTMLVisitor, Heading, RenderHTMLOptions(..), Section, buildOverrides, emptyVisitor, insertAttribute, parse, renderHtml)
import Prelude

import Data.Maybe (Maybe(..))
import Data.String (toLower, trim)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

sectionRenderer :: Section -> HTMLRenderer -> String
sectionRenderer node ctx = ctx.renderAstNodeDefault $ insertAttribute node "id" (toLower)

headingRenderer :: Heading -> HTMLRenderer -> String
headingRenderer node ctx = ctx.renderAstNodeDefault node

emphRenderer :: Emph -> HTMLRenderer -> String
emphRenderer node ctx = "<span class='emphasis'>" <> (ctx.renderChildren node) <> "</span>"

testVisitor :: HTMLVisitor -> HTMLVisitor
testVisitor v = buildOverrides $ v
  { emph = Just emphRenderer
  , section = Just sectionRenderer
  , heading = Just headingRenderer
  }

testOptions :: RenderHTMLOptions
testOptions = RenderHTMLOptions
  { overrides: Just $ testVisitor emptyVisitor
  , warn: Nothing
  }

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  let
    text = "# Test that LOWER _works_"
    ast = parse text Nothing
  it "renders" do
    let html = renderHtml ast Nothing
    trim html `shouldEqual`
      """<section id="Test-that-LOWER-works">
<h1>Test that LOWER <em>works</em></h1>
</section>"""
  it "works with visitors" do
    let html = renderHtml ast $ Just testOptions
    trim html `shouldEqual`
      """<section id="test-that-lower-works">
<h1>Test that LOWER <span class='emphasis'>works</span></h1>
</section>"""
