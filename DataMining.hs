{-#LANGUAGE ScopedTypeVariables #-}
module DataMining where

import System.IO
import DataMiningTypes hiding (evaluate)
import Control.Exception
import qualified Data.Text.Lazy.IO as LIO
import qualified Data.Text.Lazy as L
hw1 = "hw1datLERS.txt"

main :: IO ()
main = do
  mH <- getFileName
  case mH of
     Nothing -> do
       putStrLn "Goodbye!\n"
       return ()
     Just h  -> do 
       contents <- LIO.hGetContents h
       let linels = L.lines contents
       let linels' = removeCommentsAndTrailingWhites linels 
       print linels'
       --let linels'' = map trim linels'
  return ()
  
removeCommentsAndTrailingWhites :: [L.Text] -> [L.Text]
removeCommentsAndTrailingWhites [] = []
removeCommentsAndTrailingWhites (ln:lns) = let x = (L.strip $ L.takeWhile (\c -> c /= '!') ln) in 
                                             if L.length x == 0 -- we remove lines that are entirely comments
                                              then (removeCommentsAndTrailingWhites lns)
                                              else x : (removeCommentsAndTrailingWhites lns)
{-Repeatedly asks for a valid file name-}  
getFileName :: IO (Maybe Handle)  
getFileName = handle (\(e :: IOException) ->do
                         print e
                         putStrLn $ "In other words, try again!\n"
                         getFileName) $ do
      putStrLn $ "Please enter the input data file ('q' to quit):"
      str <- getLine
      case str of
       "q" -> return Nothing
       "Q" -> return Nothing 
       a@_ -> do 
         h <- openFile str ReadMode
         return (Just h)