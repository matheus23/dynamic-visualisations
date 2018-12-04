module Widgets.Record where

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
import qualified Data.Map.Lazy as Map
import Control.Lens

import qualified Widgets.TextField as TextField
import qualified Widgets.Button as Button

data Model a
  = Record Button.Model [Association a]
  deriving (Show, Eq)

data Association a
  = Association
  { associationTextField :: TextField.Model
  , associationValue :: a
  , associationAddButton :: Button.Model
  , associationRemoveButton :: Button.Model
  , associationErrors :: [Error]
  } deriving (Show, Eq)

data AssociationEvent
  = AddAssociation
  | RemoveAssociation

type Error = String

data Settings a
  = Settings
  { textStyle :: TextStyle
  , newInstance :: a
  , viewInner :: a -> Reactive Input a
  , placeholderText :: String
  , emptyRecord :: Form
  }

empty :: Model a
empty = Record Button.construct []

activeAssociation :: a -> Association a
activeAssociation inner =
  Association
    { associationTextField = TextField.emptyActive
    , associationValue = inner
    , associationAddButton = Button.construct
    , associationRemoveButton = Button.construct
    , associationErrors = []
    }

construct :: [(String, a)] -> Model a
construct = Record Button.construct . map makeModel
  where
    makeModel (str, model) = (activeAssociation model) { associationTextField = TextField.inactive str }

view :: Settings a -> Model a -> Reactive Input (Model a)
view settings (Record emptyButton []) =
    handleEvent <$> Button.view (emptyRecord settings) emptyButton
  where
    handleEvent (buttonModel, buttonClicked)
      | buttonClicked = Record buttonModel [activeAssociation (newInstance settings)]
      | otherwise     = Record buttonModel []
view settings (Record button associations) =
    Record button <$> Reactive.onVisual addBox (viewAssociations settings associations)
  where
    addBox = addBorder darkGrey . padded 4

viewAssociations :: Settings a -> [Association a] -> Reactive Input [Association a]
viewAssociations settings associations =
    appendTo down
      (Reactive.viewList
        (Reactive.processEvent (Just . handleAssociationEvents) . viewAssociation settings)
        associations)
  where
    handleAssociationEvents (association, event) =
      case event of
        Nothing -> [association]
        Just AddAssociation -> [association, activeAssociation (newInstance settings)]
        Just RemoveAssociation -> []

viewAssociation :: Settings a -> Association a -> Reactive Input (Association a, Maybe AssociationEvent)
viewAssociation settings association@(Association textField innerValue buttonAdd buttonRemove errors) =
  appendTo down
    [ appendTo right $ map (alignHV (0, 0.5)) [ nameReactive, innerReactive, buttons ]
    , Reactive.static (viewErrors settings isAssociationActive errors)
    ]
  where
    isAssociationActive =
      or [TextField.isActive textField, Button.isActive buttonAdd, Button.isActive buttonRemove]

    innerReactive =
      fmap (\value -> (association { associationValue = value }, Nothing))
        (viewInner settings innerValue)

    actionButton name buttonModel =
      alignHV (0, 0)
        (appendTo right
          [ Button.view (text (textStyle settings) name) buttonModel
          , Reactive.static (gap 4 0)
          ]
        )
    
    buttons =
      if isAssociationActive then appendTo right [ buttonAddReactive, buttonRemoveReactive ] else mempty

    buttonAddReactive =
      Reactive.processEvent
        (\(model, clicked) -> Just (association { associationAddButton = model }, if clicked then Just AddAssociation else Nothing))
        (actionButton " + " buttonAdd)

    buttonRemoveReactive =
      Reactive.processEvent
        (\(model, clicked) -> Just (association { associationRemoveButton = model }, if clicked then Just RemoveAssociation else Nothing))
        (actionButton " X " buttonRemove)

    possiblyAddBorder =
      if not (null errors) then addBorder red else id

    nameReactive =
      appendTo right
        [ Reactive.static (text (textStyle settings) ": ")
        , Reactive.onVisual (possiblyAddBorder . padded 2)
            (Reactive.processEvent (\textField -> Just (association { associationTextField = textField}, Nothing))
              (TextField.view (textStyle settings) (placeholderText settings) textField))
        ]

viewErrors :: Settings a -> Bool -> [Error] -> Form
viewErrors settings _ [] = mempty
viewErrors settings False errors = mempty
viewErrors settings True errors =
   appendTo down (map (padded 3 . addBackground red . padded 1 . text style) errors)
  where
    style = (textStyle settings) { textColor = lightGrey, fontSize = 10 }


inputValidation :: Model a -> Model a
inputValidation (Record button associations) = Record button (checkAssociations associations)

checkAssociations :: [Association a] -> [Association a]
checkAssociations assocs =
    map checkAssociation assocs
  where
    checkAssociation association =
        association
          { associationErrors = if nameAlreadyExists then ["Duplicate name"] else []
          }
      where
        associationName = assocName association
        nameAlreadyExists = length (filter (== associationName) existingNames) > 1

    assocName = TextField.getContent . associationTextField
    existingNames = map assocName assocs
