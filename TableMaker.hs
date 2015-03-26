{-#LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module TableMaker where

import System.IO
import DataMiningTypes hiding (evaluate)
import Control.Exception
import qualified Data.Text.Lazy.IO as LIO
import qualified Data.Text.Lazy as L
import Data.Char
import Control.Monad
import qualified Data.Text as Text
import Text.Read


t1IO = mkTestTable "lers1.txt"
mkTestTable str = do 
       h <- openFile str ReadMode
       c <- LIO.hGetContents h 
       let contents = removeComments c 
       let eitherTPair = extractVariablesFromText contents
       case eitherTPair of
        Left err -> do
         putStrLn err 
         putStrLn "Exiting!"
         return (Table [] [] ) -- (Left err)
        Right (variables,rest) -> do 
         let eitherHeaders = extractHeadersFromText rest 
         case eitherHeaders of 
          Left err -> do 
           --putStrLn err 
           --putStrLn "Exiting!"
           return (Table [] [] ) -- (Left err)
          Right (headers,textData) -> do
           --putStrLn ("successfully read in headers: "++ (show headers))
           let numPerRow = length headers
           let typeIndicators = (map (\h -> if (snd h) == Attribute 
                                                             then DoubleT 
                                                             else StringT) headers)  --special case known for This project.
           let eitherRows = mkRowsLERS typeIndicators (L.words textData) 1
           case eitherRows of 
            Left err -> do 
              --putStrLn $ "Error making rows: " ++ err ++ "\n Exiting!\n"
              return (Table [] [] ) -- (Left $ "Error making rows: " ++ err)
            Right rows -> do 
              --putStrLn $ "successfully read in data: " ++ (show rows)
              return (Table headers rows)
              
              
              
askAndCreateTable :: IO (Either String Table)
askAndCreateTable = do 
  mH <- getFileName
  case mH of
     Nothing -> do
       --putStrLn "Goodbye!\n"
       return (Left "User Exit\nGoodbye!\n")
     Just h  -> do 
       putStrLn "parsing data.."
       c <- LIO.hGetContents h
       let contents = removeComments c 
       let eitherTPair = extractVariablesFromText contents
       case eitherTPair of
        Left err -> do
         --putStrLn err 
         --putStrLn "Exiting!"
         return (Left err)
        Right (variables,rest) -> do 
         let eitherHeaders = extractHeadersFromText rest 
         case eitherHeaders of 
          Left err -> do 
           --putStrLn err 
           --putStrLn "Exiting!"
           return (Left err)
          Right (headers,textData) -> do
           --putStrLn ("successfully read in headers: "++ (show headers))
           let numPerRow = length headers
           let typeIndicators = (map (\h -> if (snd h) == Attribute 
                                                             then DoubleT 
                                                             else StringT) headers)  --special case known for This project.
           let eitherRows = mkRowsLERS typeIndicators (L.words textData) 1
           case eitherRows of 
            Left err -> do 
              --putStrLn $ "Error making rows: " ++ err ++ "\n Exiting!\n"
              return (Left $ "Error making rows: " ++ err)
            Right rows -> do 
              --putStrLn $ "successfully read in data: " ++ (show rows)
              return (Right (Table headers rows))

mkRowsLERS :: [TypeIndicator] -> [L.Text] -> Int -> Either String [Row]
mkRowsLERS _ [] _ = Right []
mkRowsLERS types xs i = let numPerRow = length types in
 if length xs < numPerRow 
  then 
   Left "Error. Data cases is not multiple of headers"
  else
   let (r,rest) = splitAt numPerRow xs in
    let rs = mkRowsLERS types rest (i+1) in
      case rs of 
        Left err   -> Left err 
        Right rows -> Right ( (mkRow types (i,(map L.unpack r))):rows)
        --mkRow :: [TypeIndicator] -> (Int, [String]) -> Row
extractHeadersFromText :: L.Text -> Either String ([Header], L.Text)
extractHeadersFromText mytext = let (h1,h2) = L.break (=='[') mytext in 
           if L.length h2 == 0
            then Left "Error. No '[' found in file to delimit the column titles. Exiting"
            else
             --h2 is all text starting with '['
             let (h1',h2') = L.break (==']') h2 in 
              if L.length h2' == 0 
               then Left $ "Error. No ']' found in file to delimit the column titles. Exiting."
               else
               --h1' is our header data starting with '['.
               --h2' is the the rest of our data starting with ']'
                let headerTextls = L.words (L.dropWhile (\c -> isSpace c || c == '[') h1')
                    attNum = (length headerTextls) -1 --we are told always 1 decision
                    headers = map (\(x,y) -> (L.unpack x,(if y<=attNum then Attribute else Decision))) (zip headerTextls [1..])
                    jumbledTextData = L.dropWhile (\c -> isSpace c || c == ']') h2' in
                Right (headers,jumbledTextData)
         
extractVariablesFromText :: L.Text -> Either String (L.Text,L.Text)
extractVariablesFromText text = let (t1,t2) = L.break (=='<') text in
       if L.length t2 == 0 
        then Left $ "Error. no '<' found to delimit variables. Exiting"
        else
         let (t1',t2') = L.break (=='>') t2 in 
         if L.length t2' == 0 
          then Left $ "Error. no '>' found to delimit variables. Exiting."
          else
           let variables = L.dropWhile (\c -> isSpace c || c == '<') t1' in 
            Right (variables, L.dropWhile (\c -> isSpace c || c == '>') t2')
           --t2' is the rest of our data starting with '>'


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
                                              
removeComments :: L.Text -> L.Text
removeComments inText = L.unlines $ map (L.takeWhile (\c -> c /= '!')) (L.lines inText)      
                                        
{-Repeatedly asks for a valid file name-}  
getFileName :: IO (Maybe Handle)  
getFileName = handle (\(e :: IOException) ->do
                         print e
                         putStrLn $ "In other words, try again!\n"
                         getFileName) $ do
      putStr $ "\nPlease enter the input data file ('q' to quit): "
      str <- getLine
      case str of
       "q" -> return Nothing
       "Q" -> return Nothing 
       a@_ -> do 
         h <- openFile str ReadMode
         return (Just h)
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         




trim :: String -> String
trim ('\t':xs) = trim xs 
trim (' ':xs) = trim xs
trim ls = ls 


 
readInTable :: String -> IO (Either String Table)
readInTable fileLoc = do 
   handle <- openFile fileLoc ReadMode
   contents <- hGetContents handle
   let linels = lines contents
   let linels' = map trim linels
   if (length linels') < 3 
    then  do 
      let str = "ERROR: input file is too short!"
      putStrLn str
      return (Left str) 
    else do
     let (l1:l2:l3:rest) = linels'
     case (readMaybe l1 :: Maybe Int) of
       Nothing        -> do 
         let str = "ERROR: Unable to read first line: " ++ l1 ++ " as Int (number of decisions)" 
         putStrLn str 
         return (Left str)
       (Just numDecs) -> do
          let typesinStrLS = map Text.unpack $ Text.split (==',') (Text.pack l2)   
          let typesinStrLS' = map trim typesinStrLS
          let maybes = map readMaybe typesinStrLS'
          let maybeLS = sequence maybes
          case maybeLS of
            Nothing      -> do 
              let str = "ERROR: Unable to read second line as TypeIndicators"
              putStrLn str 
              return (Left str)
            (Just types) -> do 
              --rest :: [Text]
              let rest' = map (map Text.unpack) (map (Text.split (==',')) (map Text.pack rest))
              let rest'' = map (map trim) rest'
              let headers = (map Text.unpack (Text.split (==',') (Text.pack l3)))
              let headers' = map trim headers
              mkTable headers' types numDecs rest''   



mkTable :: [String] ->[TypeIndicator] -> Int -> [[String]] -> IO (Either String Table)
mkTable headers types decNumber dat = if (length headers) /= (length types) then return (Left "TypeIndicatos/header length mismatch") 
                                                                            else do 
   let maybeRowLength = rowsSameLength dat
   case maybeRowLength of
     (Nothing)  -> return (Left "ERROR: ROW length inconsistent.")
     (Just len) -> do
        case (length headers) of
          len -> do
            let attNum = len - decNumber
            if attNum > 0 then do
                           let headers' = map (\(x,y) -> (x,(if y<=attNum then Attribute else Decision))) (zip headers [1..])
                           let dat' = mkData dat types
                           return (Right (Table headers' dat'))
                          else return (Left "Too many decision attributes listed.")
          _   -> return (Left "header length does not match row length.")
           



mkData :: [[String]] -> [TypeIndicator] -> [Row]
mkData [] _       = []
mkData lsls types = map (mkRow types) (zip [1..] lsls)

mkRow :: [TypeIndicator] -> (Int, [String]) -> Row
mkRow _ (i, []) = (i,[])
mkRow (t:ts) (i,(x:xs)) = 
   let mVal = (case t of 
               IntT    -> (case readMaybe x :: Maybe Int of
                              Nothing    -> Nothing
                              (Just int) -> (Just (IntVal int)))
               StringT -> (if (length x) > 0 then (Just (StrVal x))
                                             else Nothing)
               BoolT   -> (case readMaybe x :: Maybe Bool of
                  Nothing  -> Nothing
                  (Just b) -> (Just (BoolVal b)) )
               DoubleT -> (case readMaybe x :: Maybe Double of
                  Nothing  -> Nothing 
                  Just d   -> Just (DoubleVal d)) ) 
   in (i,(mVal:(snd (mkRow ts (i,xs)))))

                                    
                                                   

rowsSameLengthT :: [[L.Text]] -> Maybe Int
rowsSameLengthT []     = Just 0
rowsSameLengthT (x:[]) = Just (length x)
rowsSameLengthT (x:xs) = case rowsSameLengthT xs of
                             Nothing    -> Nothing
                             (Just len) -> if (length x) == len then (Just len)
                                                                else Nothing                                                   

rowsSameLength :: [[String]] -> Maybe Int
rowsSameLength []     = Just 0
rowsSameLength (x:[]) = Just (length x)
rowsSameLength (x:xs) = case rowsSameLength xs of
                             Nothing    -> Nothing
                             (Just len) -> if (length x) == len then (Just len)
                                                                else Nothing           