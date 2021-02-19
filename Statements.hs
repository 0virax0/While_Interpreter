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
    evalS (While b s1) s = fix 100 (While b s1) Skip s s                    -- Sds[while b do S]s = Fix F 

    -- put a statement inside another statement F(F)
    compose :: Statement -> Statement -> Statement
    -- This composes a while, I keep concatenating statements until I reach a negative guard; to signal that I assign the termCond variable
    compose (While b sta1) sta2  = If b (Concatenation sta1 sta2) (Assignment "termCond" (Avalue 1))        -- in case we have a while

    -- from f, current composed statement, initial state and previous state, I get the final one
    fix :: Int -> Statement -> Statement -> State -> State -> State
    fix maxIter f sta s0 sn | maxIter == 0 = sn
                    | (lookupVariable "termCond" sn1 /= Nothing) && sn == sn1 = sn1     -- I reached a fix point, return final state if the termination guard is false otherwise keep going forever
                    | otherwise = fix (maxIter-1) f sta1 s0 sn1     -- Do another application of f
                    where sta1 = compose f sta                      -- compose F with the rest of the chain to get the statement of order +1
                          sn1 = evalS sta1 s0                       -- Iteration f(n+1)

    -- Here I use the while semantics defined as D[While b do S]T = B[!b] (lfp(\T' -> T U(D[s] . B[b]T']))). I observe that the lfp gives me the invariant of the cycle and B[!b] lfp
    -- gives the exiting condition. The strongest invariant for the cycle, given the precondition T (e.g T[x->2, y->3]) is given by all combinations of states x,y that i get during computation.
    -- E.g. while (x>0) do (x:=x-1; y:=y+1), the invariant given T is [x->2, y->3] V [x->1, y->4] V [x->0, y->5]. The exiting condition is [x<=0] And [x->2, y->3] V [x->1, y->4] V [x->0, y->5] =
    -- = [x->0, y->5]. The only thing I care about is the exiting condition, so I can discard all intermediate states, leaving B[!b] (lfp(\T' -> D[s] . B[b]T'])) starting from T.
    -- I model this iterating on previous states until I reach a fixpoint (old state = this state), at this point I can have 2 situations: the cycle terminates so trivially the guard is false
    -- in this case I can return the current state as exiting condition. The second case is when the cycle does not terminate e.g (while true do skip) the fixpoint iteration stops early but the
    -- guard is true, so doind (!true) And (some exiting state) gives the bottom element or undefined. In this case the exercise states I should keep going so I added a condition for this in the
    -- fixpoint function. If the fixpoint iteration continues for ever, I certantly have an infinite loop, because to keep going I need ever new states, to do that I need to execute D[s] but to 
    -- do that I need the guard always true, so I loop.