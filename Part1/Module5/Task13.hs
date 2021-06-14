main' =
  let askName = do
        putStrLn "What is your name?"
        putStr "Name: "
        name <- getLine
        if null name then askName else return name
   in do
        name <- askName
        putStrLn $ "Hi, " ++ name ++ "!"