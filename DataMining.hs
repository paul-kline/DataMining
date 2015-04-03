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
import Data.Function
import Text.Printf
import Data.Function.Memoize
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
         let s0 = DataMiningState table Nothing []
         let dec = head $ extractFromHeaders ( tableHeaders table ) Decision
         (t,s) <- runStateT (performIntervalDisc EqualWidth dec) s0 
         putStr $ show t
         return ()
        'b' -> do 
         let s0 = DataMiningState table Nothing []
         let dec = head $ extractFromHeaders ( tableHeaders table ) Decision
         (t,s) <- runStateT (performIntervalDisc EqualFrequency dec) s0 
         putStr $ show t
         return ()
        'c' -> do 
         return ()
         
performIntervalDisc :: IntervalMethod -> String -> MyStateTMonad Table 
performIntervalDisc meth dec = do 
  s <- get
  let table = tableState s 
      headerPairs = tableHeaders table
      attributes = extractFromHeaders headerPairs Attribute
  t' <- discretizeTable meth (zip attributes (repeat 2))
  let (x,y) = l t' dec 
  liftIO $ putStrLn $ "First L: " ++ (show (x,y))
  if x == y then return t'
   else do 
    finalTable <- continueDiscretizing meth dec 
    liftIO $ putStrLn $ "table before merging:\n" ++ (show finalTable)
    let finalTable' = performPossibleMerges finalTable dec 
    liftIO $ putStrLn $ "table AFTER merging:\n" ++ (show finalTable')
    return finalTable
continueDiscretizing :: IntervalMethod -> String -> MyStateTMonad Table
continueDiscretizing meth dec = do     
 s <- get 
 let table = case discretizingTable s of 
                Nothing -> tableState s 
                Just q  -> q 
                
 let prs = map (\att -> (att, m (length (tableData table)) (map (\is -> (map (\i -> getMVal (tableHeaders table) dec (getRow table i)) is))
                                                            (compPartG table [att])))) 
                                                              (extractFromHeaders (tableHeaders table) Attribute)
     --prs = filter (\(att,mvalue) -> case  prs 
     max = maximumBy (compare `on` snd) prs 
 liftIO $ putStrLn (show prs)     
 liftIO $ putStrLn $ "Worst: " ++ (show max) 
 liftIO $ getLine
 t' <- discretizeTablekp1 meth [fst max] --this updates the kstate as well.
 let (x,y) = l t' dec 
 liftIO $ putStrLn (show (x,y))
 liftIO $ getLine
 if x == y then return t'
           else continueDiscretizing meth dec 
discretizeTablekp1 :: IntervalMethod -> [(String)] -> MyStateTMonad Table
discretizeTablekp1 meth ls = do
  s <- get 
  let kstate = discretizingState s 
      newls = foldr (\a acc -> case lookup a kstate of
                                  Nothing             -> acc 
                                  Just (k,intervals)  -> (a, k +1): acc ) [] ls
  liftIO $ putStrLn $ "Looking for: " ++ (show (head ls)) ++ " in " ++ (show kstate)
  discretizeTable meth newls                                   
discretizeTable ::IntervalMethod -> [(String,Int)] -> MyStateTMonad Table  
discretizeTable meth attkLS  = do 
  s <- get
  let origtable = tableState s
  let table = case discretizingTable s of 
                Nothing -> origtable
                Just q  -> q 
                
  let attributes = map fst attkLS
  let columnValsLS = map (\(a,k) -> ((getColumnValsNoMabies' origtable) a,k)) attkLS
  let columnMValsLS = map (extractColumn' origtable) attributes
      allColumnNames = map fst (tableHeaders table)
      --decisionColumns = map (extractColumn' table) (extractFromHeaders (tableHeaders table) Decision)
   -- oldColumnAndCutPoints :: [([Maybe Value], [(Double,Double)]]    (header,[cutpoints])
      intervalsLS = (map (\(a,k) -> (calcIntervals' meth k a)) columnValsLS)
      newAttKIntervalsLS = zip attkLS intervalsLS
      oldColumnAndCutPoints =zip columnMValsLS intervalsLS
      newColumns = (map (\(old, cuts) -> discretizeColumn old cuts) oldColumnAndCutPoints)
      newColsWithNames = zip attributes newColumns
      cols = map (\att -> case lookup att newColsWithNames of 
                            Nothing -> extractColumn' table att 
                            Just v  -> v ) allColumnNames
      rows = mytranspose cols
      table' = replaceRows table rows
      kInfo = discretizingState s
      ksub = filter (\(att,_) -> not(att `elem` attributes)) kInfo 
      newAdditions = map (\((att,k),intervals) -> (att, (k,intervals))) newAttKIntervalsLS
      kInfo' = newAdditions ++ ksub 
  liftIO $ putStrLn $ "new intervals: " ++ (show intervalsLS)
  put $ DataMiningState origtable (Just table') kInfo'
  liftIO $ putStrLn (show table')
  liftIO $ getLine
  return table'
  
discretizeColumn :: [Maybe Value] -> [(Double,Double)] -> [Maybe Value]  
discretizeColumn [] _ = []
discretizeColumn a@((Just (StrVal _)):_) _  =  a 
discretizeColumn a@((Just (BoolVal _)):_) _ =  a
discretizeColumn column intervals = map (categorize intervals) column 

categorize :: [(Double,Double)] -> Maybe Value -> Maybe Value 
categorize intervals (Just (DoubleVal d))  =   let mInterval = find (\pair -> d >= (fst pair) && d <= (snd pair)) intervals in 
                                        case mInterval of 
                                           Nothing -> Just $ StrVal "ERROR"
                                           Just i  ->Just $ Interval (fst i) (snd i)
categorize intervals (Just (IntVal i)) = categorize intervals (Just (DoubleVal (fromIntegral i)))
categorize _ a@_ = a 
                                           
calcIntervals :: IntervalMethod -> Int -> [Maybe Value] -> [(Double,Double)]
calcIntervals meth x ls = calcIntervals' meth x (noMaybies ls)

calcIntervals' :: IntervalMethod -> Int -> [Value] -> [(Double,Double)]
calcIntervals' EqualWidth = calcEqualWidthIntervals
calcIntervals' EqualFrequency = calcEqualFrequencyIntervals

calcEqualFrequencyIntervals :: Int -> [Value] -> [(Double,Double)]
calcEqualFrequencyIntervals _ ((StrVal _):_) = []
calcEqualFrequencyIntervals _ ((BoolVal _):_) = []
calcEqualFrequencyIntervals n ls = let ls' = foldr (\x acc -> case x of 
                                                               (DoubleVal d) -> d : acc  
                                                               (IntVal i   ) -> (fromIntegral i):acc ) [] ls
                                       intervals = (calcEqualFrequencyIntervalshelper n ls')
                                       adjacents = zip intervals (tail intervals)
                                       cuts = map (\(i1,i2) -> ((last i1) + (head i2))/ 2) adjacents
                                       middles = zip cuts (tail cuts)
                                       in 
                                    ((head (head intervals),head cuts):middles) ++ [((last cuts), (last (last intervals)) )]
calcEqualFrequencyIntervalshelper :: (Ord a, Fractional a) => Int -> [a] -> [[a]]
calcEqualFrequencyIntervalshelper n ls =
                                   let ls' = sort ls
                                       uniques = sort $ nub ls' 
                                       occs = zip uniques (map ((flip occurances) ls') uniques)
                                       total = length ls'
                                       posscuts = map (\(x,y) -> ( x + y ) /2 ) (zip uniques (tail uniques)) 
                                       cutGroups =map sort $ combinations n posscuts
                                       appliedCuts = map (applyCuts ls') cutGroups
                                       measured = map (measure idealBlockLength) appliedCuts
                                       idealBlockLength =  (fromIntegral total) / (fromIntegral n)
                                       choosefromThese = zip appliedCuts measured 
                                       (intervals,value) = minimumBy (compare `on` snd) choosefromThese  
                                       --ls = zip intervals (tail intervals)
                                       --answer = map (\(i1,i2) -> 
                                       in 
                                   map toList (equalFrequencyHelper n occs) where --intervals where
                                     measure ideal lsls = sumBy (\x -> (abs((fromIntegral (length x)) - ideal))**2) lsls                                                                
applyCuts :: (Ord a) => [a] -> [a] -> [[a]]
applyCuts ls cs = let ls' = sort ls 
                      cs' = sort cs
                      in 
                  f ls' cs' where 
                     f xs [] = [xs] 
                     f xs cuts = let c = head cuts 
                                     (x,y) = span (\p -> p<=c) xs 
                                     in 
                                 x: (f y (tail cuts))
                                 
-- a,Int list is assumed ordered!!                                   
equalFrequencyHelper :: Int -> [(a,Int)] -> [[(a,Int)]]
equalFrequencyHelper _ [] = [] 
equalFrequencyHelper 1 xs = [xs] 
equalFrequencyHelper n xs | n >= (length xs) = map listify xs
                          | otherwise = 
                            let total = sumBy snd xs 
                                idealBlockLength =  (fromIntegral total) / (fromIntegral n)
                                len = length xs                                
                                x = zip [1..] (map (\c -> sumBy snd (take c xs)  ) [1..len] ) 
                                bestpair = minimumBy (compare `on` (\q -> abs ( (fromIntegral (snd q)) - idealBlockLength))) x
                                --bestpair is now (takeThismany,distanceFromIdealBlock)
                                dropNum = adjust (fst bestpair) xs 
                                in
                            (take (dropNum) xs): (equalFrequencyHelper (n -1) (drop (dropNum) xs)) where
                            --adjust handles the case where we have grouped a number of things together, but now we don't have
                            -- enough cutpoints to satisfy to number we were asked.
                              adjust i ls = if (length (drop i ls)) < (n -1)
                                              then adjust (i -1) ls 
                                              else i 
calcEqualWidthIntervals' :: Int -> [Maybe Value] -> [(Double,Double)]
calcEqualWidthIntervals' n ls = calcEqualWidthIntervals n (noMaybies ls)

listify :: a -> [a]
listify x = [x]

testEqF :: [Int] -> Int -> [[Int]]
testEqF ls n =map toList $ equalFrequencyHelper n (zip ([1..]::[Int]) ls)

toList :: [(a,Int)] -> [a]
toList [] = []
toList ((x,i):xs) = (take i (repeat x)) ++ (toList xs)


sumBy ::(Num b) => (a -> b) -> [a] -> b 
sumBy f ls = foldr (\x acc -> (f x) + acc) 0 ls 
                                            
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
    
    
    
slow_fib :: Int -> Integer
slow_fib 0 = 0
slow_fib 1 = 1
slow_fib n = (memoized_fib (n-2)) + (memoized_fib (n-1))

memoized_fib :: Int -> Integer
memoized_fib = memoize slow_fib 