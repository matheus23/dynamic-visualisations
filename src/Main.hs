{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Graphics.Declarative.Transforms
import Graphics.Declarative.Bordered
import qualified Graphics.Declarative.Border as Border
import Graphics.Declarative.Cairo.TangoColors
import Graphics.Declarative.Cairo.Form
import Graphics.Declarative.Cairo.Shape
import Graphics.Declarative.SDL.Input

import qualified Reactive
import Reactive (Reactive(..))
import qualified Event
import RunReactive (runReactive)
import Utils (orElse, isInside, orTry, rightAngle)
import Linear
import FormUtils
import Data.List (intersperse)
import Data.Maybe (mapMaybe)
import Control.Lens
import qualified Debug.Trace as Debug


main :: IO ()
main =
  runReactive viewWithSidepanel (State (V2 0 0) (Let [Assoc "point" (Vector (V2 10 10))] (Pic mempty)) NotDragging)


-- TODO: Sized reactives
viewWithSidepanel :: State -> Reactive Input State
viewWithSidepanel model =
  mconcat
    [ viewCode model
    , move (V2 600 300) (viewGraphics model)
    ]

data State = State
  { mousePos :: V2 Double
  , expression :: Let Literal
  , dragState :: DragState
  }
  deriving Show

data Let a
  = Let [Assoc a] a
  deriving (Functor, Show)

data Assoc a = Assoc String a
  deriving (Functor, Show)

data Literal
  = Vector (V2 Double)
  | Pic Form

instance Show Literal where
  show (Vector v) = show v
  show (Pic _) = "<form>"

data DragState
  = NotDragging
  | DraggingPoint Int (V2 Double)
  deriving Show

textStyle :: TextStyle
textStyle = defaultTextStyle { fontFamily = "monospace" }


keywordStyle :: TextStyle
keywordStyle = textStyle { textColor = lightBlue, bold = True }


monoText :: String -> Form
monoText = text textStyle



-- Rendering


renderLets :: Let Literal -> Form
renderLets (Let definitions body) =
    appendTo down
      [ letKeyword <> move (V2 greaterWidth 0) (appendTo down (map (renderAssoc . fmap renderLit) definitions))
      , inKeyword <> move (V2 greaterWidth 0) (renderLit body)
      ]
  where
    greaterWidth = max (graphicWidth letKeyword) (graphicWidth inKeyword)
    letKeyword = text keywordStyle "let "
    inKeyword = text keywordStyle "in "


renderLit :: Literal -> Form
renderLit (Pic form) = form
renderLit (Vector vec) = renderVec vec


renderAssoc :: Assoc Form -> Form
renderAssoc (Assoc name form) =
  appendTo right [ monoText name, monoText " = ", form ]


renderVec :: V2 Double -> Form
renderVec (V2 x y) = monoText (show (x, y))


renderApp :: Form -> [Form] -> Form
renderApp func args =
  appendTo right (intersperse (monoText " ") (func:args))


viewGraphicallyScope :: State -> Reactive Input State
viewGraphicallyScope state =
  let
    Let definitions literal = expression state
  in
  mconcat (zipWith (viewGraphicallyAssoc state) [0..] definitions)


viewGraphicallyAssoc :: State -> Int -> Assoc Literal -> Reactive Input State
viewGraphicallyAssoc state index (Assoc name (Vector v)) =
  let
    grabbingRange = 10

    isInGrabbingRange = distance v (mousePos state) < grabbingRange

    grabbable =
      outlined (solid lightBlue) (circle 6)
    
    nonGrabbable =
      filled lightBlue (circle 6)
    
    Let definitions literal = expression state

    updateAssociation index newAssoc =
      state { expression = Let (set (element index) newAssoc definitions) literal }
    
    handleDragRelease grabPoint pos =
      (updateAssociation index (Assoc name (Vector (mousePos state - grabPoint))))
        { dragState = NotDragging }

    handleDragStart pos =
      state { dragState = DraggingPoint index pos }
  in
  case dragState state of
    NotDragging ->
      if isInGrabbingRange then
        move v
          (Reactive
            ((Event.mousePress . Event.buttonGuard MBLeft) (Just . handleDragStart))
            grabbable)
      else
        move v (Reactive.static nonGrabbable)
    
    DraggingPoint dragIndex grabPoint ->
      if dragIndex == index then
        move (mousePos state - grabPoint)
          (Reactive
            ((Event.mouseRelease . Event.buttonGuard MBLeft) (Just . handleDragRelease grabPoint))
            grabbable)
      else
        move v (Reactive.static nonGrabbable)
viewGraphicallyAssoc mousePos index _ = mempty



-- View code


viewCode :: State -> Reactive Input State
viewCode state =
    Reactive.static (move padding renderedCode <> background)
  where
    padding = V2 8 8
    renderedCode = renderLets (expression state)
    background = alignHV (0, 0) (filled lightGrey (rectangle 400 600))


viewGraphics :: State -> Reactive Input State
viewGraphics state =
  Reactive.onEvent
    (Event.handleChain
      [ onRelease (\pos -> Just (state { expression = literalActionAt pos (expression state), mousePos = pos }))
      , onMove (\pos -> Just (state { mousePos = pos }))
      ])
    (mconcat
      [ viewGraphicallyScope state
      -- , Reactive.static (renderLit literal)
      , Reactive.static originCross
      ])
  where
    -- onPress = Event.mousePress . Event.buttonGuard MBLeft
    onRelease = Event.mouseRelease . Event.buttonGuard MBLeft
    onMove = Event.mouseMove

    bakePath = outlined (solid darkGrey) . noBorder . openPath
    line start end = bakePath (pathPoint start `lineConnect` pathPoint end)
    originCross = line (-100, 0) (100, 0) <> line (0, -100) (0, 100)


literalActionAt :: V2 Double -> Let Literal -> Let Literal
literalActionAt position (Let definitions literal) =
  let
    points =
      mapMaybe 
        (\case
          Assoc _ (Vector vector) -> Just vector
          _ -> Nothing)
        definitions

    findDistance index point = (index, distance point position)

    nearPoints = filter ((< 10) . snd) (zipWith findDistance [0..] points)
  in
  if null nearPoints then
    Let (Assoc "point" (Vector position) : definitions) literal
  else
    Let definitions literal
