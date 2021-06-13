data Log a = Log [String] a deriving (Show)

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log aMsg a) f = Log newMsg b
  where
    (Log bMsg b) = f a
    newMsg = aMsg ++ bMsg
