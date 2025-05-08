{-# LANGUAGE OverloadedStrings#-}

module P4Types where

data Expression = Expression {
    self :: String
    -- , exprValue :: (Maybe String)
    , exprType :: Maybe String
    } deriving (Show, Eq, Ord)
type Statement = String

ppP4E :: Expression -> String
ppP4E x = self x