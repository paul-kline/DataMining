{-#LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
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
       let eitherTable = mkTableFromLERS linels'
       print linels'
       print eitherTable
       --let linels'' = map trim linels'
  return ()

mkTableFromLERS :: [L.Text] -> Either String Table
mkTableFromLERS [] = Left "No lines to read! was the file empty?"
mkTableFromLERS (ln1:ln2:lns) = let eitherHeaders = mkHeadersLERS ln2 in 
  case eitherHeaders of 
   Left err -> Left err 
   Right headers -> let dat = mkDataLERS (map (\h -> if (snd h) == Attribute 
                                                             then DoubleT 
                                                             else StringT) headers) lns in 
     Left $ "successfully got headers: " ++(show headers) ++ "Here is the data:\n" ++ (show dat)

     
mkDataLERS :: [TypeIndicator] -> [L.Text]-> [Row]
mkDataLERS types lins = let lnls = zip [1..] lins in 
  map (\(i,ln) ->(mkRow types)  (i,(map L.unpack (L.words ln))) ) lnls   
                           

--mkRow :: [TypeIndicator] -> (Int, [String]) -> Row

      
mkHeadersLERS :: L.Text -> Either String [Header]
mkHeadersLERS lin = case (L.isPrefixOf "[" lin, L.isSuffixOf "]" lin) of
                  (False,False) -> Left "ERROR making headers: no '[' found starting second line. No ']' found ending second line"
                  (False,True)  -> Left "ERROR making headers: No ']' found ending second line"
                  (True, False) -> Left "ERROR making headers: no '[' found starting second line."
                  (True,True)   -> let x =(L.stripSuffix "]") lin in 
                    case x of 
                      Nothing -> Left "Error making headers: Found '[' and ']' surrounding second line, but somehow failed to remove them. You should never see this error"
                      Just nosuff -> let mline = L.stripPrefix "[" nosuff in 
                        case mline of 
                          Nothing -> Left "Error making headers: Found '[' and ']' surrounding second line, but somehow failed to remove them. You should never see this error"
                          Just lin' -> let ls = L.words (L.strip lin') in
                            let attNum = (length ls) - 1 in
                            Right $ map (\(x,y) -> (L.unpack x,(if y<=attNum then Attribute else Decision))) (zip ls [1..])

removeCommentsAndTrailingWhites :: [L.Text] -> [L.Text]
removeCommentsAndTrailingWhites [] = []
removeCommentsAndTrailingWhites (ln:lns) = let x = (L.strip $ L.takeWhile (\c -> c /= '!') ln) in 
                                             if L.length x == 0 -- we remove lines that are entirely comments
                                              then     (removeCommentsAndTrailingWhites lns)
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