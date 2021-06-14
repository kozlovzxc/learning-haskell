import Control.Monad
import Data.String (fromString)
import Data.Text (Text, isInfixOf)
import System.Directory (doesFileExist, getDirectoryContents, removeFile)

listFiles = do
  entities <- getDirectoryContents "."
  filterM doesFileExist entities

main' :: IO ()
main' = do
  putStr "Substring: "
  key <- getLine
  if not $ null key then removeFile key else putStrLn "Canceled"
  where
    removeFile key = do
      files <- listFiles
      matchedFiles <- filterM (return . isInfixOf (fromString key) . fromString) files
      mapM_
        ( \x -> do
            putStrLn $ "Removing file: " ++ x
            -- removeFile x
        )
        matchedFiles
