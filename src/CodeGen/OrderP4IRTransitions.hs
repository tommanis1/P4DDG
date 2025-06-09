module CodeGen.OrderP4IRTransitions where
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module CodeGen.Continuations where

import qualified Transducer.Def as T
import P4Types
import Data.Graph.Inductive hiding(Node, Edge, Graph)
import qualified DDG.P4DDG hiding (Stmt)
import DDG.Types (Grammar, Label(..), Rule(..), Param(..), Nonterminal(..))
import Data.List (intercalate, find, partition)

type P4Transducer =
    T.Transducer
    ([Stmt P4Types.Statement])
    (Transition (DDG.P4DDG.E P4Types.Expression))

data Transition e =
    Otherwise | E e deriving (Show)

data Stmt stmt =
    Push Integer
    | Pop Integer
    | Extract String
    | Bind String String
    | HostLanguageStmts [stmt] deriving (Show)