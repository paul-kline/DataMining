{-#LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module DataMining where

import System.IO
import DataMiningTypes hiding (evaluate)
import Control.Exception
import qualified Data.Text.Lazy.IO as LIO
import qualified Data.Text.Lazy as L
import Data.Char
import Control.Monad
import TableMaker
import DataMiningTypes
import DataMiningTools
import Control.Monad.State
import Data.List 
hw1 = "hw1datLERS.txt"

main :: IO ()
main = do
  eitherTable <- askAndCreateTable
  case eitherTable of 
    Left err -> do
     putStrLn err 
     putStrLn "Exiting"
    Right table -> do 
     putStrLn (show table)
     putStrLn "Your file is acceptable." 
     mmethod <- chooseDiscMethod
     case mmethod of 
      Nothing -> do
       putStrLn "Goodbye!"
       return ()
      Just m -> do
       putStrLn $ "Method: " ++ (show m)
       case m of 
        'a' -> do 
         let s0 = DataMiningState table
         let dec = head $ extractFromHeaders ( tableHeaders table ) Decision
         (t,s) <- runStateT (performEqualWidthDisc dec) s0 
         putStr $ show t
         return ()
        'b' -> do 
         return ()
        'c' -> do 
         return ()
         
performEqualWidthDisc :: String -> MyStateTMonad Table 
performEqualWidthDisc dec = do 
  s <- get
  let table = tableState s 
      headerPairs = tableHeaders table
      attributes = extractFromHeaders headerPairs Attribute
      t' = discretizeTable table attributes EqualWidth 2
      (x,y) = l t' dec 
  if x == y then return t'
   else do 
                  
    return t'
  
discretizeTable :: Table -> [String] -> IntervalMethod -> Int -> Table  
discretizeTable table attributes EqualWidth k = let columnValsLS = map (getColumnValsNoMabies' table) attributes in
  let columnMValsLS = map (extractColumn' table) attributes
      decisionColumns = map (extractColumn' table) (extractFromHeaders (tableHeaders table) Decision)
   -- oldColumnAndCutPoints :: [([Maybe Value], [(Double,Double)]]    (header,[cutpoints])
      oldColumnAndCutPoints =zip columnMValsLS (map (calcEqualWidthIntervals k) columnValsLS)
      newColumns = (map (\(old, cuts) -> discretizeColumn old cuts) oldColumnAndCutPoints) ++ decisionColumns
      rows = mytranspose newColumns in 
      replaceRows table rows


discretizeColumn :: [Maybe Value] -> [(Double,Double)] -> [Maybe Value]  
discretizeColumn [] _ = []
discretizeColumn a@((Just (StrVal _)):_) _  =  a 
discretizeColumn a@((Just (BoolVal _)):_) _ =  a
discretizeColumn column intervals = map (categorize intervals) column 

categorize :: [(Double,Double)] -> Maybe Value -> Maybe Value 
categorize intervals (Just (DoubleVal d))  =   let mInterval = find (\pair -> d >= (fst pair) && d <= (snd pair)) intervals in 
                                        case mInterval of 
                                           Nothing -> Just $ StrVal "ERROR"
                                           Just i  ->Just $ StrVal $ (show (fst i)) ++ ".." ++ (show (snd i))
categorize intervals (Just (IntVal i)) = categorize intervals (Just (DoubleVal (fromIntegral i)))
categorize _ a@_ = a 
                                           
                                   
calcEqualWidthIntervals :: Int -> [Value] -> [(Double,Double)]
calcEqualWidthIntervals numintervals columnvals = do 
  if length columnvals == 0 then []
  else do
    let mv = (case head columnvals of 
         (StrVal _)     -> Nothing 
         (BoolVal _)    -> Nothing
         (IntVal i)     -> (Just (fromIntegral i))
         (DoubleVal d)  -> Just d)
    case mv of 
      Nothing -> []
      Just v  -> do 
         let max = case maximum columnvals of 
                     IntVal i -> fromIntegral i
                     DoubleVal d -> d 
             min = case minimum columnvals of
                     IntVal i -> fromIntegral i 
                     DoubleVal d -> d              
         let width = (max - min)/((fromIntegral numintervals))
         map (\(x,y) -> ((min + (fromIntegral x)*width),(min + (fromIntegral y)*width))) (zip [0..numintervals] [1..numintervals])
chooseDiscMethod :: IO (Maybe Char)
chooseDiscMethod = do 
 putStrLn "Please choose your desired global discretization method ('q' to quit):"
 putStrLn "\t(a) equal interval width"
 putStrLn "\t(b) equal frequency per interval"
 putStrLn "\t(c) conditional entropy"
 putStr $ ": "
 str <- getLine
 case str of 
  "q" -> do
    --putStrLn "Exiting"
    return Nothing
  "Q" -> do
    --putStrLn "Exiting"
    return Nothing
  "a" -> return $ Just 'a'
  "A" -> return $ Just 'a'      
  "b" -> return $ Just 'b'
  "B" -> return $ Just 'b'
  "c" -> return $ Just 'c'
  "C" -> return $ Just 'c'
  _ -> do 
    putStrLn "INVALID SELECTION.\n"
    chooseDiscMethod