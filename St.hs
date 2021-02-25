module St where
    import qualified Data.Map.Strict as Map
    -- I define the state as a map from keys(variables names) to values
    type Vname = String
    data Vvalue = Num Integer | Bool Integer | Undefined deriving (Show, Eq, Read) 
    type State = Map.Map Vname Vvalue
    -- lookup variable telling also if the value was not found
    lookupVariable :: Vname -> State -> Maybe Vvalue
    lookupVariable = Map.lookup 
    -- I can then read a state s x, searching for the key in the state
    readState :: State -> Vname -> Vvalue
    readState s x = handleMissing(Map.lookup x s)
    -- missing values are returned as undefined
    handleMissing :: Maybe Vvalue -> Vvalue
    handleMissing Nothing = Undefined
    handleMissing (Just v) = v
    -- I do a substitution as s' = s[y -> v]
    substituteState :: State -> Vname -> Vvalue -> State
    substituteState s _ Undefined = s
    substituteState s n v | lookupVariable n s /= Nothing = Map.update (\_->Just v) n s
                          | otherwise = Map.insert n v s



