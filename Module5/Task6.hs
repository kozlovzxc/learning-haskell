data Log a = Log [String] a deriving (Show)

returnLog :: a -> Log a
returnLog = Log []