import qualified Data.Map.Strict as Map
import St
import Statements
import Parser

-- get program syntax
execProgram :: String -> [(Vname, Vvalue)] -> State
execProgram prog l = Map.delete "termCond"( evalS (parseStatement (parseSyntax prog)) (Map.fromList l))

-- execProgram "for (i(:=)0); (i(<)3); (i(+=)1) do (a(+=)1)" [("a",1)]
-- execProgram "while (a(<)b) do (a(+=)1)" [("a",1),("b",5)]
-- execProgram "repeat (a(+=)1) until (a(<)b)" [("a",0),("b",5)]
-- execProgram "while (true) do (skip)" []