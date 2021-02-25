module Statements where
    import qualified Data.Map.Strict as Map
    import St
    -- an expression starts from a state and gives a value
    class Expression e where
        evalE :: e -> State -> Vvalue

    -- define arithmetic and boolean expressions
    data Aexpr = Avalue Vvalue | Aname Vname | Sum Aexpr Aexpr | Sub Aexpr Aexpr | Mul Aexpr Aexpr 
        deriving Show
    eSum :: Vvalue -> Vvalue -> Vvalue
    eSum Undefined _ = Undefined  
    eSum _ Undefined = Undefined  
    eSum (Num n1) (Num n2) = Num (n1+n2)
    eMul :: Vvalue -> Vvalue -> Vvalue
    eMul Undefined _ = Undefined  
    eMul _ Undefined = Undefined  
    eMul (Num n1) (Num n2) = Num (n1*n2)
    eMin :: Vvalue -> Vvalue -> Vvalue
    eMin Undefined _ = Undefined  
    eMin _ Undefined = Undefined  
    eMin (Num n1) (Num n2) = Num (n1-n2)
    instance Expression Aexpr where
        evalE (Avalue v) _ = v                             -- N[n]s = N[n]
        evalE (Aname n) s = readState s n                  -- A[x]s = sx
        evalE (Sum a1 a2) s = eSum (evalE a1 s) (evalE a2 s)  -- A[a1+a2]s = A[a1]s + A[a2]s
        evalE (Sub a1 a2) s = eMin (evalE a1 s) (evalE a2 s) -- A[a1-a2]s = A[a1]s - A[a2]s
        evalE (Mul a1 a2) s = eMul (evalE a1 s) (evalE a2 s)  -- A[a1*a2]s = A[a1]s * A[a2]s

    tt = Bool 1
    ff = Bool 0
    data Bexpr = Bvalue Vvalue | Bname Vname | And Bexpr Bexpr | Or Bexpr Bexpr | Not Bexpr | 
                Eql Aexpr Aexpr | Gt Aexpr Aexpr | Lt Aexpr Aexpr
        deriving Show
    egt :: Vvalue -> Vvalue -> Vvalue
    egt Undefined _ = Undefined  
    egt _ Undefined = Undefined  
    egt (Num n1) (Num n2) | b == True = tt
                          | otherwise = ff
                            where b = n1 > n2
    elt :: Vvalue -> Vvalue -> Vvalue
    elt Undefined _ = Undefined  
    elt _ Undefined = Undefined  
    elt (Num n1) (Num n2) | b == True = tt
                          | otherwise = ff
                            where b = n1 < n2
    instance Expression Bexpr where
        evalE (Bvalue vv) _ = vv                                 -- B[tt]s = tt
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
        evalE (Gt a1 a2) s = egt a3 a4 
                        where a3 = evalE a1 s
                              a4 = evalE a2 s
        evalE (Lt a1 a2) s = elt a3 a4
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
    evalS (While b s1) s = iter (While b s1) s

    iter :: Statement -> State -> State
    iter w s | evalE b s == ff = s 
             | otherwise = iter w (evalS body s)
             where While b body = w