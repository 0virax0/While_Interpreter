module Main where
    import WhileLang
    import St

    main = do sta <- getLine 
              st <- parseState 
              let result = execProgram sta st
              print result

    parseState :: IO [(Vname, Vvalue)]
    parseState = do a <- getLine
                    case a of
                     "_" -> return []
                     a1 -> (do v <- getLine 
                               re <- parseState
                               case v of
                                   "undefined" -> return ((a1,Undefined):re)
                                   v1 -> return ((a1,Num (read v1 :: Integer)):re))
