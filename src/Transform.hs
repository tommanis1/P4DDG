P4TransducerGraph = Gr Stmt Transition

data Stmt = If String (Transition, Node) 

-- In p4 reading input must happen in a state
moveTerminalTransitions

-- a -> b -> c can => abc, all except with more than one incoming transition
concatTransitions 

{- moveGuardedTransitions  -} should happen in p4 code 

runConfig :: Transducer -> P4Transducer
runConfig t =  mk t [moveTerminalTransitions, concatTransitions, moveGuardedTransitions]

mk :: Transducer -> [P4Transducer -> P4Transducer] -> P4Transducer 
mk t fs = fold id t fs