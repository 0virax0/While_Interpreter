module Statements where
    import qualified Data.Map.Strict as Map
    import St
    -- an expression starts from a state and gives a value
    class Expression e where
        evalE :: e -> State -> Vvalue

    -- define arithmetic and boolean expressions
    data Aexpr = Avalue Vvalue | Aname Vname | Sum Aexpr Aexpr | Sub Aexpr Aexpr | Mul Aexpr Aexpr 
        deriving Show
    instance Expression Aexpr where
        evalE (Avalue v) _ = v                             -- N[n]s = N[n]
        evalE (Aname n) s = readState s n                  -- A[x]s = sx
        evalE (Sum a1 a2) s = evalE a1 s + evalE a2 s  -- A[a1+a2]s = A[a1]s + A[a2]s
        evalE (Sub a1 a2) s = evalE a1 s - evalE a2 s  -- A[a1-a2]s = A[a1]s - A[a2]s
        evalE (Mul a1 a2) s = evalE a1 s * evalE a2 s  -- A[a1*a2]s = A[a1]s * A[a2]s

    tt = 1
    ff = 0
    data Bexpr = Bvalue Vvalue | Bname Vname | And Bexpr Bexpr | Or Bexpr Bexpr | Not Bexpr | 
                Eql Aexpr Aexpr | Gt Aexpr Aexpr | Lt Aexpr Aexpr
        deriving Show
    instance Expression Bexpr where
        evalE (Bvalue 1) _ = 1                                  -- B[tt]s = tt
        evalE (Bvalue 0) _ = 0
        evalE (Bname n) s = readState s n                       -- B[x]s = sx
        evalE (And b1 b2) s | (b3 == tt) && (b4 == tt) = tt     -- B[b1 && b2]s = { tt if B[b1]s == tt and B[b2] == tt
                            | otherwise = ff                                          --ff otherwise}
                        where b3 = evalE b1 s
                              b4 = evalE b2 s
        evalE (Or b1 b2) s | (b3 == tt) || (b4 == tt) = tt 
                           | otherwise = ff 
                        where b3 = evalE b1 s
                              b4 = evalE b2 s
        evalE (Not b1) s | b3 == tt = ff 
                         | otherwise = tt 
                        where b3 = evalE b1 s
        evalE (Eql a1 a2) s | a3 == a4 = tt 
                            | otherwise = ff
                        where a3 = evalE a1 s
                              a4 = evalE a2 s
        evalE (Gt a1 a2) s | a3 > a4 = tt 
                           | otherwise = ff
                        where a3 = evalE a1 s
                              a4 = evalE a2 s
        evalE (Lt a1 a2) s | a3 < a4 = tt 
                           | otherwise = ff
                        where a3 = evalE a1 s
                              a4 = evalE a2 s

    -- define Statement 
    data Statement = Skip | Assignment Vname Aexpr | Concatenation Statement Statement | If Bexpr Statement Statement | While Bexpr Statement   deriving Show

    evalS :: Statement -> State -> State
    evalS Skip s = s                                                    -- id
    evalS (Assignment var val) s = substituteState s var (evalE val s)  -- s[var -> A[val]s]
    evalS (Concatenation st1 st2) s = (evalS st2 . evalS st1) s         -- Sds[st2]s . Sds[st1]s
    evalS (If b st1 st2) s | evalE b s == tt = evalS st1 s              -- {st1 s if b s == tt
                           | otherwise = evalS st2 s                    --  st2 s if b s == ff}
    evalS (While b s1) s = iter (While b s1) Skip s s                    -- Sds[while b do S]s = Fix F 

    -- put a statement inside another statement F(F)
    compose :: Statement -> Statement -> Statement
    -- I keep concatenating statements until I reach a negative guard; to signal that I assign the termCond variable
    compose (While b sta1) sta2  = If b (Concatenation sta1 sta2) (Assignment "termCond" (Avalue 1))        -- in case we have a while

    -- from f, current composed statement, initial state and previous state, I get the final state and the last composed statement
    iter_ :: Statement -> Statement -> State -> State -> (State, Statement)
    iter_ f sta s0 sn | (lookupVariable "termCond" sn1 /= Nothing) && sn == sn1 = (sn1, sta)     --  return final state and final composed statement if the termination guard is false otherwise keep going forever
                      | otherwise = iter_ f sta1 s0 sn1               -- do all another applications of f
                      where sta1 = compose f sta                      -- do another composition of the statements
                            sn1 = evalS sta1 s0                       -- evaluate the new composition

    iter :: Statement -> Statement -> State -> State -> State
    iter f sta s0 sn = sf
                        where (sf, staf) = iter_ f sta s0 sn 