{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
module Site.Static.Css (css) where

import Prelude hiding (div, rem, span)

import Control.Monad      (forM_)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Text          (pack)
import Clay

import qualified Clay.Media as M


--------------------------------------------------------------------------------

darkAndDusty ∷ Color
darkAndDusty = "#1a1a1a"

darkAndDustyInputBackground ∷ Color
darkAndDustyInputBackground = "#232729"

disabledGray ∷ Color
disabledGray = "#9c9c9c"

inputBorder ∷ Color
inputBorder = "#33393b"

dustyGrayBlue ∷ Color
dustyGrayBlue = "#374553"

comfortableText ∷ Color
comfortableText = "#ddd"

comfortableTextWithEmphasis ∷ Color
comfortableTextWithEmphasis = "#ccc"

niceBlue ∷ Color
niceBlue = "#7390ba"

niceBlueHover ∷ Color
niceBlueHover = "#3e6cae"

niceGreen ∷ Color
niceGreen = "#30573c"

niceGreenHover ∷ Color
niceGreenHover = "#284a31"

niceYellow ∷ Color
niceYellow = "#e39b40"

niceYellowHover ∷ Color
niceYellowHover = "#c87f23"

borderRadius2px ∷ Css
borderRadius2px = borderRadius (px 2) (px 2) (px 2) (px 2)


--------------------------------------------------------------------------------

boxShadowWith ∷ Double → Color → Css
boxShadowWith blurRadius c =
  boxShadow $ (bsColor c $ shadowWithBlur (px 0) (px 0) (px blurRadius)) :| []


--------------------------------------------------------------------------------

_tintOf ∷ Color → Css
_tintOf clr = before & do
  content    $ stringContent ""
  display    block
  position   absolute
  top        (px 0)
  bottom     (px 0)
  left       (px 0)
  right      (px 0)
  background $ setA 0.5 clr
  transition "all" (sec 0.2) linear (sec 0)

  hover & do
    background $ setA 0 clr


--------------------------------------------------------------------------------

toplevel ∷ Css
toplevel = do
  (html <> body) ? do
    height          (pct 100)
    width           (pct 100)
    margin          (px 0) (px 0) (px 0) (px 0)
    padding         (px 0) (px 0) (px 0) (px 0)
    left            (px 0)
    top             (px 0)
    fontSize        (pct 100)
    backgroundColor darkAndDusty

  a ? do
    textDecoration none
    color          niceYellow
    hover          & color niceYellowHover


--------------------------------------------------------------------------------

topnav ∷ Css
topnav = "#topnav" ? do
  a ? do
    color transparent
    hover & color transparent
    img   ? width (px 16)


--------------------------------------------------------------------------------

typography ∷ Css
typography = do
  star ? do
    fontFamily    ["Tahoma", "Arial"] [sansSerif]
    textRendering optimizeLegibility
    color         comfortableText
    lineHeight    (em 1.5)

  (h1 <> h2 <> h3 <> h4 <> h5)
    ? color comfortableTextWithEmphasis

  h1 ? fontSize (rem 2.000)
  h2 ? fontSize (rem 1.375)
  h3 ? fontSize (rem 1.125)
  h4 ? fontSize (rem 1.000)
  h5 ? fontSize (rem 0.875)

  p ? do
    fontSize   (rem    1)
    fontWeight (weight 200)
    lineHeight (em     1.8)

  ".font-light"   ? fontWeight (weight 300)
  ".font-regular" ? fontWeight (weight 400)
  ".font-heavy"   ? fontWeight (weight 700)

  ".left"    ? textAlign (alignSide sideLeft)
  ".right"   ? textAlign (alignSide sideRight)
  ".justify" ? textAlign justify
  ".center"  ? do
    textAlign   (alignSide sideCenter)
    marginLeft  auto
    marginRight auto

  queryOnly M.screen [M.minWidth (px 720)] $ do
    h1 ? fontSize (rem 2.500)
    h2 ? fontSize (rem 2.000)
    h3 ? fontSize (rem 1.375)
    h4 ? fontSize (rem 1.125)
    h5 ? fontSize (rem 1.000)
    p  ? fontSize (rem 1.125)


--------------------------------------------------------------------------------

pageContainer ∷ Css
pageContainer = do
  "#page-container" ? do
    minHeight (pct 100)
    position  relative

  "#top-stripe" ? do
    height          (px  2)
    width           (pct 100)
    margin          (px 0) (px 0) (px 0) (px 0)
    padding         (px 0) (px 0) (px 0) (px 0)
    left            (px 0)
    top             (px 0)
    backgroundColor niceBlueHover

  "#page" ? padding (px 10) (px 10) (px 60) (px 10)

  "#footer-stripe" ? do
    backgroundColor dustyGrayBlue
    position        absolute
    bottom          (px 0)
    width           (pct 100)
    maxWidth        (pct 100)
    textAlign       (alignSide sideCenter)
    fontSize        (em 0.8)
    boxShadowWith   12 dustyGrayBlue

    div # ("class" ^= "col") ? do
      "margin" -: "0rem 2%"

    "#a-top" ? do
      color comfortableTextWithEmphasis
      float floatRight


--------------------------------------------------------------------------------

gridSystem ∷ Css
gridSystem = do
  -- Based loosely on https://github.com/zachacole/Simple-Grid
  ".grid-container" ? do
    width       (pct 90)
    marginLeft  auto
    marginRight auto

  ".row" ? do
    position relative
    width    (pct 100)

    div # ("class" ^= "col") ? do
      float     floatLeft
      minHeight (rem 0.125)
      "margin"  -: "0.2rem 2%"

      ".right-if-wide-enough" & marginRight (px 0)

      (textarea <> input <> button) ? do
        width     (pct 100)
        boxSizing borderBox


    after & do
      content $ stringContent ""
      display displayTable
      clear   both

  -- .col-1, .., .col-12 { width: 96% }
  let cs = ((".col-" <>) . pack . show) <$> [1..12 ∷ Int]
  element (intercalate ", " cs) ? width (pct 96)

  ".col-1-sm"  ? width (pct 04.33)
  ".col-2-sm"  ? width (pct 12.66)
  ".col-3-sm"  ? width (pct 21.00)
  ".col-4-sm"  ? width (pct 29.33)
  ".col-5-sm"  ? width (pct 37.66)
  ".col-6-sm"  ? width (pct 46.00)
  ".col-7-sm"  ? width (pct 54.33)
  ".col-8-sm"  ? width (pct 62.66)
  ".col-9-sm"  ? width (pct 71.00)
  ".col-10-sm" ? width (pct 79.33)
  ".col-11-sm" ? width (pct 87.66)
  ".col-12-sm" ? width (pct 96.00)

  ".hidden-sm" ? display none

  -- min-width: 540px
  queryOnly M.screen [M.minWidth (px 540)] $ do
    ".grid-container" ? width (pct 80)

  -- min-width: 720px
  queryOnly M.screen [M.minWidth (px 720)] $ do
    ".col-1"     ? width   (pct 04.33)
    ".col-2"     ? width   (pct 12.66)
    ".col-3"     ? width   (pct 21.00)
    ".col-4"     ? width   (pct 29.33)
    ".col-5"     ? width   (pct 37.66)
    ".col-6"     ? width   (pct 46.00)
    ".col-7"     ? width   (pct 54.33)
    ".col-8"     ? width   (pct 62.66)
    ".col-9"     ? width   (pct 71.00)
    ".col-10"    ? width   (pct 79.33)
    ".col-11"    ? width   (pct 87.66)
    ".col-12"    ? width   (pct 96.00)
    ".hidden-sm" ? display block

    ".right-if-wide-enough" ? do
      textAlign   (alignSide sideRight)
      marginRight (px 0)

    ".grid-container" ?
      ".row" ?
        ".col-float-right" ? float floatRight

  -- min-width: 960px
  queryOnly M.screen [M.minWidth (px 960)] $ do
    ".grid-container" ? do
      width    (pct 75)
      maxWidth (rem 60)


--------------------------------------------------------------------------------

controlWidgets ∷ Css
controlWidgets = do
  let vPad = px 4

  ".right-if-wide-enough" ? paddingTop vPad
  label                   ? color      niceBlue

  (input <> textarea) ? do
    background    darkAndDustyInputBackground
    borderColor   inputBorder
    borderStyle   solid
    borderWidth   (em 0.05)
    boxShadowWith 2 dustyGrayBlue
    color         comfortableTextWithEmphasis
    fontSize      (em 1.1)
    padding       vPad (px 8) vPad (px 8)
    borderRadius2px

  let mkBtn (suffix, bcol, hcol) = element ("a.btn-" <> suffix) ? do
        background     bcol
        borderTop      (px 1) solid bcol
        boxShadowWith  12 dustyGrayBlue
        color          white
        display        block
        fontSize       (em 1)
        textAlign      center
        textDecoration none
        verticalAlign  middle
        borderRadius2px

        active & background hcol

        hover & do
          background hcol
          cursor     pointer

        ".disabled" & do
          background disabledGray
          borderTop  (px 1) solid disabledGray
          active     & background disabledGray
          hover      & background disabledGray

  flip forM_ mkBtn [ ("green", niceGreen, niceGreenHover)
                   , ("blue",  niceBlue,  niceBlueHover ) ]


--------------------------------------------------------------------------------

codeBlocks ∷ Css
codeBlocks = "div.sourceCode" ? do
  -- Based on "solarized dark" https://github.com/altercation/solarized
  pre ? do
    overflowX auto

    code ? do
      color "#657B83"

      span ? do
        color      comfortableText
        fontFamily [] [monospace]
        hover      & color comfortableText
        before     & textDecoration none

        let mkSpan cls clr stl = element ("span." <> cls) ? do
              color      clr
              fontFamily [] [monospace]
              fontStyle  stl
              fontWeight normal

         -- solBase0   = "#839496"
         -- solBase00  = "#657b83"
         -- solBase02  = "#073642"
         -- solBase03  = "#002b36"
         -- solBase2   = "#eee8d5"
         -- solBase3   = "#fdf6e3"
         -- solMagenta = "#d33682"
         -- solViolet  = "#6c71c4"
            solBase01  = "#586e75"
            solBase1   = "#93a1a1"
            solBlue    = "#268bd2"
            solCyan    = "#2aa198"
            solGreen   = "#719e07"
            solOrange  = "#cb4b16"
            solRed     = "#dc322f"
            solYellow  = "#b58900"

        mkSpan "kw" solGreen  normal -- Keywords
        mkSpan "dt" solRed    normal -- DataType
        mkSpan "dv" solBlue   normal -- DecVal
        mkSpan "bn" solCyan   normal -- BaseN
        mkSpan "fl" solCyan   normal -- Float
        mkSpan "ch" solCyan   normal -- Char
        mkSpan "st" solCyan   normal -- String
        mkSpan "co" solBase01 italic -- Comment
        mkSpan "ot" solOrange normal -- Other
        mkSpan "al" solRed    normal -- Alert
        mkSpan "fu" solBlue   normal -- Function
        mkSpan "er" solRed    normal -- Error
        mkSpan "wa" solRed    italic -- Warning
        mkSpan "cn" solOrange normal -- Constant
        mkSpan "sc" solRed    normal -- SpecialChar
        mkSpan "vs" solYellow normal -- VerbatimString
        mkSpan "ss" solRed    normal -- SpecialString
        mkSpan "im" solYellow normal -- Import
        mkSpan "va" solBlue   normal -- Variable
        mkSpan "cf" solGreen  normal -- ControlFlow
        mkSpan "op" solGreen  normal -- Operator
        mkSpan "bu" solYellow normal -- BuiltIn
        mkSpan "ex" solYellow normal -- Extension
        mkSpan "pp" solGreen  normal -- Preprocessor
        mkSpan "at" solYellow normal -- Attribute
        mkSpan "do" solBase1  italic -- Documentation
        mkSpan "an" solBase1  italic -- Annotation
        mkSpan "cv" solBase1  italic -- CommentVar
        mkSpan "in" solBase1  italic -- Information

--------------------------------------------------------------------------------

spinner ∷ Css
spinner = do
  -- Based on https://github.com/tobiasahlin/SpinKit

  ".sk-folding-cube-container" ? do
    margin    (px 20) auto (px 20) auto
    width     (px 40)
    height    (px 40)
    position  relative
    transform (rotateZ (deg 45))

    ".sk-cube" ? do
      float     floatLeft
      width     (pct 50)
      height    (pct 50)
      position  relative
      transform (scaleY 1.1)

      before & do
        content            $ stringContent ""
        position           absolute
        top                (px 0)
        left               (px 0)
        width              (pct 100)
        height             (pct 100)
        backgroundColor    comfortableText
        "transform-origin" -: "100% 100%"

        animation "sk-fold-cube-angle"
                  (sec 2.4)
                  linear
                  (sec 0)
                  infinite
                  normal
                  forwards

    ".sk-cube-2" ? do
      transforms [ scaleY 1.1, rotateZ (deg  90) ]
      before     & animationDelay (sec 0.3)

    ".sk-cube-3" ? do
      transforms [ scaleY 1.1, rotateZ (deg 180) ]
      before     & animationDelay (sec 0.6)

    ".sk-cube-4" ? do
      transforms [ scaleY 1.1, rotateZ (deg 270) ]
      before     & animationDelay (sec 0.9)

  let frameA = do
        transforms [ perspective (140), rotateX (deg (-180)) ]
        opacity 0

      frameB = do
        transforms [ perspective (140), rotateX (deg 0) ]
        opacity 1

      frameC = do
        transforms [ perspective (140), rotateY (deg 180) ]
        opacity 0


  keyframes "sk-fold-cube-angle"
            [ (000.0, frameA), (010.0, frameA)
            , (025.0, frameB), (075.0, frameB)
            , (090.0, frameC), (100.0, frameC) ]


--------------------------------------------------------------------------------

tiltOnHover ∷ Css
tiltOnHover = ".tilt-on-hover" ? do
  let frameA = transforms [ scaleY 1.3, scaleX 1.3 ]
      frameB = transforms [ rotate (deg   20)      ]
      frameC = transforms [ rotate (deg (-10))     ]

  keyframes "tilt-on-hover-frames"
            [ (020.00, frameA)
            , (060.00, frameB)
            , (100.00, frameC)
            ]

  hover & do
    animation "tilt-on-hover-frames"
              (sec 0.7)
              linear
              (sec 0)
              (iterationCount 1)
              normal
              forwards

--------------------------------------------------------------------------------

homePage ∷ Css
homePage = "#root-home" ? do
  img ? do
    display       block
    marginLeft    auto
    marginRight   auto
    boxShadowWith 0 transparent

  ("#logo-github" <> "#icon-mail" <> "#icon-secured") ? do
    color transparent
    hover & color transparent
    img   ? width (px 64)

  ("#logo-haskell" <> "#logo-purescript") ? do
    display       inlineBlock
    height        (px 24)
    width         (px 24)
    verticalAlign middle

  queryOnly M.screen [M.minWidth (px 720)] $ do
    p ? marginTop (px 0)

    ("#row-github" <> "#row-contact" <> "#row-gpg") ? marginTop (px 20)

    "#row-github"  ? "#logo-github"  ? float floatRight
    "#row-contact" ? "#icon-mail"    ? float floatLeft
    "#row-gpg"     ? "#icon-secured" ? float floatRight


--------------------------------------------------------------------------------

contactForm ∷ Css
contactForm = "#root-contact" ? do
  ".row" ? paddingTop (px 10)
  input  ? maxWidth   (px 280)

  "#comment-body" ? do
    "resize"  -: "vertical"
    minHeight (px 200)


--------------------------------------------------------------------------------

notFound404 ∷ Css
notFound404 = "#root-404" ? do
  h1 ? fontSize (rem 6)

  "#row-home-icon" ? do
    a ? do
      color transparent
      hover & color transparent
      img   ? width (px 64)


--------------------------------------------------------------------------------

css ∷ Css
css = do
  toplevel
  topnav
  typography
  pageContainer
  gridSystem
  controlWidgets
  codeBlocks
  spinner
  tiltOnHover
  homePage
  contactForm
  notFound404
