module Widgets.Expression where

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
import Data.Monoid (First(..))
import Utils
import Linear
import FormUtils

import qualified Widgets.TextField as TextField
import qualified Widgets.Button as Button
import qualified Widgets.DropDownList as DropDownList
import qualified Widgets.Type as Type
import qualified Widgets.Record as Record

import qualified Backend

data Model
  = Hole (DropDownList.Model Model)
  | App Model Model
  | Abs (Record.Model Type.Model) Model
  deriving (Show, Eq)

example :: Model
example = App (lam hole) hole

monoStyle :: TextStyle
monoStyle = defaultTextStyle { fontFamily = "monospace" }

recordSettings :: Record.Settings Type.Model
recordSettings = (Type.recordSettings Backend.stdTypeEnv)
  { Record.placeholderText = "parameter"
  , Record.emptyRecord = noParametersError }

noParametersError :: Form
noParametersError =
    padded 4 (addBackground red (text style "No Parameters"))
  where
    style = (Record.textStyle recordSettings) { textColor = lightGrey, fontSize = 10 }

hole :: Model
hole = Hole DropDownList.construct

lam :: Model -> Model
lam body = Abs (Record.construct [("", Type.hole)]) body

holeOptions :: [Model]
holeOptions = [App hole hole, lam hole]

holeSettings :: DropDownList.Settings Model
holeSettings = DropDownList.Settings
  { DropDownList.textStyle = Record.textStyle recordSettings
  , DropDownList.buttonText = "<hole>"
  , DropDownList.dropDownText = "Choose Expression:"
  , DropDownList.renderModel = Reactive.visual . view
  , DropDownList.updateDropdown = Hole
  }

view :: Model -> Reactive Input Model
view (Hole dropDownListModel) = DropDownList.view holeSettings holeOptions dropDownListModel
view (App functionModel argumentModel) =
    alignHV (0, 0)
      (appendTo right
        (map (alignHV (0, 0.5))
          [ updateFunction <$> view functionModel
          , Reactive.static (padded 4 applicationArrow)
          , updateArgument <$> view argumentModel
          ] ))
  where
    updateFunction newFunc = App newFunc argumentModel
    updateArgument newArg = App functionModel newArg
view (Abs parametersRecord bodyModel) =
    (appendTo down
      [ appendTo right
          [ alignHV (0, 1) (Reactive.static lambdaText)
          , Reactive.static (gap 8 0)
          , alignHV (0, 1) (updateParameters <$> Record.view recordSettings parametersRecord)
          ]
      , Reactive.static (gap 0 4)
      , appendTo right
          [ Reactive.static functionArrow
          , Reactive.static (gap 6 0)
          , updateBody <$> alignHV (0, 0) (view bodyModel)
          ]
      ])
  where
    updateParameters newParams = Abs newParams bodyModel
    updateBody newBody = Abs parametersRecord newBody

lambdaText :: Form
lambdaText = text monoStyle { textColor = blue } "λ"

functionArrow :: Form
functionArrow = text monoStyle { textColor = blue } "→"

applicationArrow :: Form
applicationArrow =
  outlined (solid darkGrey) (bordered (0, 0.5) 6 8 applicationArrowShape)

applicationArrowShape :: Shape
applicationArrowShape =
  closedPath     (pathPoint (0, -4)
    `lineConnect` pathPoint (6, 0)
    `lineConnect` pathPoint (0, 4))
