{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where

import Graphics.Declarative.Transforms
import Graphics.Declarative.Bordered
import qualified Graphics.Declarative.Border as Border
import Graphics.Declarative.Cairo.TangoColors
import Graphics.Declarative.Cairo.Form
import Graphics.Declarative.Cairo.Shape
import Graphics.Declarative.SDL.Input
import qualified Graphics.Declarative.SDL.Keys as Keys

import qualified Reactive
import Reactive (Reactive(..))
import qualified Event
import RunReactive (runReactive)
import Utils (orElse, isInside, orTry, rightAngle)
import Linear
import FormUtils
import Data.List (intersperse)
import Data.Maybe (mapMaybe, maybeToList, listToMaybe)
import Control.Lens
import Control.Monad (guard)


main :: IO ()
main =
    runReactive (move (V2 400 300) . viewGraphics) initialState

initialState = State
    { mousePos = V2 0 0
    , shiftPressed = False
    , expression = Let [] (Point (V2 10 10)) -- Let [Assoc "point" (Point (V2 10 10))] Hole
    , dragState = NotDragging
    , selection = []
    }


data State = State
    -- input states
    { mousePos :: V2 Double
    , shiftPressed :: Bool
    -- program state
    , expression :: Let Literal
    -- editing state
    , dragState :: DragState
    , selection :: [FocusLet ()]
    }
    deriving Show

data Let a = Let
    { declarations :: [Declaration a]
    , value :: a
    }
    deriving (Functor, Show)

data FocusLet a
    = FocusDeclaration String a
    | FocusIn a
    deriving (Functor, Show)

data Declaration a = Declaration
    { name :: String
    , assignment :: a
    }
    deriving (Functor, Show)

data Literal
    = Point (V2 Double)
    | Hole
    deriving Show

data DragState
    = NotDragging
    | Dragging (FocusLet ()) (V2 Double)
    deriving Show


lookupFocus :: FocusLet b -> Let a -> Maybe a
lookupFocus focus (Let declarations expression) = case focus of
    FocusDeclaration focusName a ->
        assignment <$> listToMaybe (filter ((== focusName) . name) declarations)

    FocusIn inner ->
        Just expression


modifyFocus :: FocusLet b -> (a -> a) -> Let a -> Let a
modifyFocus focus f (Let declarations expression) = case focus of
    FocusDeclaration focusName a ->
        let
            changeDecl decl =
                if name decl == focusName then
                    Declaration (name decl) (f (assignment decl))
                else
                    decl
        in
        Let (map changeDecl declarations) expression

    FocusIn _ ->
        Let declarations (f expression)


textStyle :: TextStyle
textStyle = defaultTextStyle { fontFamily = "monospace" }


keywordStyle :: TextStyle
keywordStyle = textStyle { textColor = lightBlue, bold = True }


monoText :: String -> Form
monoText = text textStyle



-- Update

viewGraphics :: State -> Reactive Input State
viewGraphics state =
    let
        scene =
            viewLet (expression state)

        handlePress pos =
            case react scene pos of
                Nothing ->
                    let
                        declarationsBefore =
                            declarations (expression state)

                        addedDeclaration =
                            Declaration
                                (makeFreshName "point" (map name declarationsBefore))
                                (Point pos)
                    in
                    Just state
                        { expression =
                            Let (declarationsBefore ++ [addedDeclaration]) (value (expression state :: Let Literal))
                        }

                Just focus ->
                    case lookupFocus focus (expression state) of
                        Just (Point position) ->
                            Just state { dragState = Dragging focus (position - pos) }

                        _ ->
                            Nothing

        updateDrag pos focus grabOffset =
            modifyFocus focus
                (const (Point (pos + grabOffset)))
                (expression state)

        handleMove pos =
            case dragState state of
                NotDragging ->
                    Nothing

                Dragging focus grabOffset ->
                    Just state { expression = updateDrag pos focus grabOffset }

        handleRelease pos =
            case dragState state of
                NotDragging ->
                    Nothing

                Dragging focus grabOffset ->
                    Just state
                        { expression = updateDrag pos focus grabOffset
                        , dragState = NotDragging
                        }
    in
    Reactive
        (Event.handleChain
            [ Event.mousePress (Event.buttonGuard MBLeft handlePress)
            , Event.mouseRelease (Event.buttonGuard MBLeft handleRelease)
            , Event.mouseMove handleMove
            ]
        )
        (mconcat
            [ Reactive.visual scene
            , viewOriginCross
            ]
        )



-- View Pickable Objects


viewLet :: Let Literal -> Reactive (V2 Double) (FocusLet ())
viewLet (Let declarations expression) =
    mconcat ((FocusIn <$> viewLiteral expression) : map viewDeclarations declarations)


viewDeclarations :: Declaration Literal -> Reactive (V2 Double) (FocusLet ())
viewDeclarations (Declaration name literal) =
    FocusDeclaration name <$> viewLiteral literal


viewLiteral :: Literal -> Reactive (V2 Double) ()
viewLiteral Hole = mempty
viewLiteral (Point v) = move v viewCross


viewCross :: Reactive (V2 Double) ()
viewCross =
    let
        cross =
            lineShape <> scale (V2 (-1) 1) lineShape

        lineShape =
            outlined (solid black) (noBorder (closedPath (pathPoint (-6, -6) `lineConnect` pathPoint (6, 6))))
    in
    Reactive
        (\pickPos -> do
            guard (norm pickPos < norm (V2 6 6))
            return ()
        )
        cross

viewOriginCross :: Form
viewOriginCross =
    let
        bakePath =
            outlined (solid darkGrey) . noBorder . openPath

        line start end =
            bakePath (pathPoint start `lineConnect` pathPoint end)
    in
    line (-100, 0) (100, 0) <> line (0, -100) (0, 100)


-- λ> makeFreshName "point" ["point1", "point2"]
-- "point3"
makeFreshName :: String -> [String] -> String
makeFreshName base names =
    let
        freshNames =
            map ((base ++) . show) [1..]

        isFresh =
            not . (`elem` names)
    in
    head (filter isFresh freshNames)
