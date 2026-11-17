module CodeGen.Unfold where

import GrammarToTransducer

-- This module implements a strategy of directly connecting accepting states with the callee

-- This only works when there are no states that have more than one incoming call edge . i.e when there is no recursion

toP4 :: Transducer -> Either UnfoldError P4

