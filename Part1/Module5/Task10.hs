import Data.Char

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace
  deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken x
  | x == "+" = Just Plus
  | x == "-" = Just Minus
  | x == "(" = Just LeftBrace
  | x == ")" = Just RightBrace
  | all isDigit x = Just $ Number $ read x
  | otherwise = Nothing

tokenize :: String -> Maybe [Token]
tokenize = sequence . map asToken . words