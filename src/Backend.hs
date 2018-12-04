module Backend where

import qualified Data.Map.Lazy as Map

data Type
  = Nat
  | Arrow Type Type
  | RecordType (Record Type)
  deriving (Show, Eq)

type Record a
  = Map.Map String a

type TypeEnv = Map.Map String Type

stdTypeEnv :: TypeEnv
stdTypeEnv = Map.fromList
  [ ("Nat", Nat)
  , ("Unit", RecordType Map.empty)
  , ("Point", RecordType (Map.fromList [("x", Nat), ("y", Nat)])) ]
