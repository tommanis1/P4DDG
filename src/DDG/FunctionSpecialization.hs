{- 
This module implements specialization for parameterized nonterminals that do not contain recursive calls and have more than one callee. 
Doing this avoids having to store something in memory to remember which callee control needs to return to. 
-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
module DDG.FunctionSpecialization where

import qualified DDG.P4DDG hiding (Stmt)
import DDG.Types (Grammar, Label(..), Rule(..), Param(..), Nonterminal(..), NonTerminalId)
import P4Types hiding (pp)

import Control.Monad.State.Lazy
type DDG = Grammar
type NonTerminalName = String
type E = (DDG.P4DDG.E P4Types.Expression)

-- Iterate
specialise :: DDG -> DDG
specialise g = 
    let
        allToSpecialise = filter (\(Nonterminal n _ _)-> not (hasSingleCallee g n) && not (isRecursive g n)) g
        (Nonterminal n _ _):xs = allToSpecialise
        specialized = specialise' g n
    in 
        if null xs then
            specialized
        else
            specialise specialized
    

specialise' :: DDG -> NonTerminalName -> DDG
specialise' g n = 
    let
        callers = filter (\(Nonterminal _ _ rule) -> 
                            n `elem` visitLabelRule collectCallName rule) g
        
        -- original definition
        (Nonterminal _ params rule) = head $ filter (\(Nonterminal name _ _) -> name == n) g
        
        gWithoutOriginal = filter (\(Nonterminal name _ _) -> name /= n) g
        
        -- Create specialized versions for each callee
        result = evalState (updateLabelDDG (updateNonterminalCall n) gWithoutOriginal) 1
        
        -- New definitions 
        specialized = map (\i -> Nonterminal (n ++ "_" ++ show i) params rule) [1..length callers]
    in
        result ++ specialized

updateNonterminalCall :: MonadState Int m => NonTerminalName -> Label e -> m (Label e)
updateNonterminalCall n (NonTerminalCall id e) =
    if id == n then do
        i <- get
        put (i + 1)
        return (NonTerminalCall (id ++ "_" ++ show i) e)
    else return (NonTerminalCall id e)
updateNonterminalCall _ l = return l

-- Some helper functions
hasSingleCallee :: DDG -> NonTerminalName -> Bool
hasSingleCallee g n = 
    let 
        calls = visitLabelDDG collectCallName g
    in 
        length (filter (== n) calls) <= 1

isRecursive :: DDG -> NonTerminalName -> Bool
isRecursive g n = 
    let
        (Nonterminal _ _ def) = head $ filter (\(Nonterminal name _ _) -> name == n) g
        calls = visitLabelRule collectCallName def
    in 
        n `elem` calls



collectCallName (NonTerminalCall id _) = [id]
collectCallName _ = []


visitLabelDDG :: (Label E -> [x]) -> DDG -> [x]
visitLabelDDG f g = concatMap (\(Nonterminal _ _ r) -> visitLabelRule f r) g

visitLabelRule f (KleineClosure r) = visitLabelRule f r
visitLabelRule f (Alternation r1 r2) = visitLabelRule f r1 ++ visitLabelRule f r2
visitLabelRule f (Sequence r1 r2) = visitLabelRule f r1 ++ visitLabelRule f r2
visitLabelRule f (Label l) = f l


updateLabelDDG :: Monad m => (Label E -> m (Label E)) -> DDG -> m DDG
updateLabelDDG f g = traverse updateNonterminal g
  where
    updateNonterminal (Nonterminal id attrs r) = do
      r' <- updateLabelRule f r
      return (Nonterminal id attrs r')

updateLabelRule :: Monad m => (Label E -> m (Label E)) -> Rule E -> m (Rule E)
updateLabelRule f (KleineClosure r) = KleineClosure <$> updateLabelRule f r
updateLabelRule f (Alternation r1 r2) = do
  r1' <- updateLabelRule f r1
  r2' <- updateLabelRule f r2
  return (Alternation r1' r2')
updateLabelRule f (Sequence r1 r2) = do
  r1' <- updateLabelRule f r1
  r2' <- updateLabelRule f r2
  return (Sequence r1' r2')
updateLabelRule f (Label l) = Label <$> f l