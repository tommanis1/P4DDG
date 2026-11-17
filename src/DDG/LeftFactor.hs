-- This module is llm generated
module DDG.LeftFactor where

import DDG.Types

-- type Grammar = [Nonterminal]

-- data Nonterminal = Nonterminal NonTerminalId [Param] (Rule (DDG.P4DDG.E P4Types.Expression)) deriving (Show)


-- data Rhs = [[]]

-- leftFactor :: [[Rhs e]] -> [[Rhs e]]
-- leftFactor [] = []


flattenAlternation :: (Eq e, Show e) => Rule e -> [Rule e]
flattenAlternation (Alternation r1 r2) = flattenAlternation r1 ++ flattenAlternation r2
flattenAlternation r = [r]

decomposeSequence :: (Eq e, Show e) => Rule e -> [Rule e]
decomposeSequence (Sequence r1 r2) = decomposeSequence r1 ++ decomposeSequence r2
decomposeSequence r = [r]

longestCommonPrefix ::  (Eq e, Show e) => [[Rule e]] -> [Rule e]
longestCommonPrefix [] = []
longestCommonPrefix (x:xs) = foldl commonPrefix x xs
  where
    commonPrefix (a:as) (b:bs)
      | a == b    = a : commonPrefix as bs
      | otherwise = []
    commonPrefix _ _ = []

leftFactor' :: (Eq e, Show e) => [[Rule e]] -> ([Rule e], [[Rule e]])
leftFactor' seqs =
    let prefix = longestCommonPrefix seqs
  in (prefix, map (drop (length prefix)) seqs)
leftFactor :: (Eq e, Show e) => Rule e -> ([Rule e], [[Rule e]])
leftFactor alt@(Alternation _ _) =
  let alts = flattenAlternation alt
      seqs = map decomposeSequence alts
  in leftFactor' seqs
leftFactor r = ([], [decomposeSequence r])

findLongestCommonPrefix ::  (Eq e, Show e) => Rule e -> [Rule e]
findLongestCommonPrefix alt@(Alternation _ _) =
  let alts = flattenAlternation alt
      seqs = map decomposeSequence alts
      prefix = longestCommonPrefix seqs
    in prefix

-- Helper to build a sequence from a list of rules
buildSequence :: (Eq e, Show e) => [Rule e] -> Rule e
buildSequence [] = Label Epsilon
buildSequence [r] = r
buildSequence (r:rs) = Sequence r (buildSequence rs)

-- Helper to rebuild a rule from prefix and suffixes
rebuildRule :: (Eq e, Show e) => [Rule e] -> [[Rule e]] -> Rule e
rebuildRule [] suffixes = foldr1 Alternation (map buildSequence suffixes)
rebuildRule prefix suffixes = 
    let alt = foldr1 Alternation (map buildSequence suffixes)
    in foldr Sequence alt prefix

-- Recursively left-factor a rule
leftFactorRule :: (Eq e, Show e) => Rule e -> Rule e
leftFactorRule (Alternation r1 r2) =
    let alts = flattenAlternation (Alternation r1 r2)
        seqs = map decomposeSequence alts
        (prefix, suffixes) = leftFactor' seqs
    in if null prefix
        then Alternation (leftFactorRule r1) (leftFactorRule r2)
        else leftFactorRule (rebuildRule prefix suffixes)
leftFactorRule (Sequence r1 r2) = Sequence (leftFactorRule r1) (leftFactorRule r2)
leftFactorRule (KleineClosure r) = KleineClosure (leftFactorRule r)
leftFactorRule (Label l) = Label l