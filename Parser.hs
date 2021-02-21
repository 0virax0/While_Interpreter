module Parser where
    import qualified Data.Map.Strict as Map
    import Statements
    import Text.Read
    -- I parse the Input building a syntax tree, from it I create the semantics
    data SyntaxTree = Terminal String | SynStatement [SyntaxTree]   deriving Show

    parseSyntax :: String -> SyntaxTree
    parseSyntax s | null split = Terminal ""
                  | length split == 1 = Terminal (filter (/=' ') (head split))
                  | otherwise = SynStatement [parseSyntax part | part <- split]
                where split = filter (/="") (splitKeyword s)

    -- I split based on words or matching parenthesis
    -- Transforms "if (b) then" into ["if", "b", "then"]
    splitKeyword :: String -> [String]
    splitKeyword "" = []
    splitKeyword [s] = [[s]]
    splitKeyword ('(':sx) = [] : splitParenthesis sx 1              -- keyword finished, search for parenthized content
    splitKeyword (s:sx) = (s:s1):sx1                                -- keep accumulating the keyword
                        where (s1:sx1) = splitKeyword sx

    splitParenthesis :: String -> Int -> [String]
    splitParenthesis "" 0 = []
    splitParenthesis "" _ = error "parenthesis do not match correctly"
    splitParenthesis [')'] 1 = [[]]
    splitParenthesis s 0 = [] : splitKeyword s                          -- no open parenthesis left, split and search for keywords
    splitParenthesis (')':xs) 1 = splitParenthesis xs 0                 -- remove last parenthesis
    splitParenthesis ('(':sx) n = ('(':s1):sx1                          -- one more opened parenthesis
                            where (s1:sx1) = splitParenthesis sx (n+1)
    splitParenthesis (')':sx) n = (')':s1):sx1                          -- found matching parenthesis
                            where (s1:sx1) = splitParenthesis sx (n-1)
    splitParenthesis (s:sx) n = (s:s1):sx1                              -- found nothing keep going
                            where (s1:sx1) = splitParenthesis sx n

    -- I transform the syntax to semantics using the syntax tree
    parseAexpr :: SyntaxTree -> Aexpr
    parseAexpr (Terminal t) | n == Nothing = Aname t         -- Avalues and Anames are contained in terminals
                            where n = readMaybe t :: Maybe Int
    parseAexpr (Terminal t) = Avalue (read t :: Int)
    parseAexpr (SynStatement [op1, Terminal "+", op2]) = Sum (parseAexpr op1) (parseAexpr op2)
    parseAexpr (SynStatement [op1, Terminal "-", op2]) = Sub (parseAexpr op1) (parseAexpr op2)
    parseAexpr (SynStatement [op1, Terminal "*", op2]) = Mul (parseAexpr op1) (parseAexpr op2)
    parseAexpr t = error ("error parsing Aexpr: " ++ show t)

    parseBexpr :: SyntaxTree -> Bexpr
    parseBexpr (Terminal t) | t=="true" = Bvalue tt
                            | t=="false" = Bvalue ff
                            | otherwise = Bname t
    parseBexpr (SynStatement [op1, Terminal "&&", op2]) = And (parseBexpr op1) (parseBexpr op2)
    parseBexpr (SynStatement [op1, Terminal "||", op2]) = Or (parseBexpr op1) (parseBexpr op2)
    parseBexpr (SynStatement [Terminal "!", op1]) = Not (parseBexpr op1) 
    parseBexpr (SynStatement [op1, Terminal "==", op2]) = Eql (parseAexpr op1) (parseAexpr op2)
    parseBexpr (SynStatement [op1, Terminal ">", op2]) = Gt (parseAexpr op1) (parseAexpr op2)
    parseBexpr (SynStatement [op1, Terminal "<", op2]) = Lt (parseAexpr op1) (parseAexpr op2)
    parseBexpr t = error ("error parsing Bexpr: " ++ show t)

    parseStatement :: SyntaxTree -> Statement
    parseStatement (Terminal "skip") = Skip
    parseStatement (SynStatement [Terminal var, Terminal ":=", val]) = Assignment var (parseAexpr val)
    parseStatement (SynStatement [Terminal var, Terminal "+=", val]) = Assignment var (Sum (parseAexpr (Terminal var)) (parseAexpr val))
    parseStatement (SynStatement [Terminal var, Terminal "-=", val]) = Assignment var (Sub (parseAexpr (Terminal var)) (parseAexpr val))
    parseStatement (SynStatement [Terminal "while", b, Terminal "do", st]) = While (parseBexpr b) (parseStatement st)

    parseStatement (SynStatement [Terminal "for", assignment, Terminal ";", b, Terminal ";", increment, Terminal "do", st]) = 
      Concatenation (parseStatement assignment) (While (parseBexpr b) (Concatenation (parseStatement st) (parseStatement increment)))
    parseStatement (SynStatement [Terminal "repeat", st, Terminal "until", b]) = Concatenation (parseStatement st) (While (Not (parseBexpr b)) (parseStatement st))
    parseStatement (SynStatement [Terminal "if", b, Terminal "then", st1, Terminal "else", st2]) = If (parseBexpr b) (parseStatement st1) (parseStatement st2)
    parseStatement (SynStatement [st1, Terminal ";", st2]) = Concatenation (parseStatement st1) (parseStatement st2)
    parseStatement s = error ("error parsing Statement: " ++ show s)
    