{-#LANGUAGE ScopedTypeVariables, OverloadedStrings, RecordWildCards #-}
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
         let s0 = DataMiningState table Nothing [] []
         let dec = head $ extractFromHeaders ( tableHeaders table ) Decision
         (t,s) <- runStateT (performIntervalDisc EqualWidth dec) s0 
         putStr $ show t
         --(unit,s') <- runStateT (writeFiles 'a' t) s 
         writeFiles t
         return ()
        'b' -> do 
         let s0 = DataMiningState table Nothing [] []
         let dec = head $ extractFromHeaders ( tableHeaders table ) Decision
         (t,s) <- runStateT (performIntervalDisc EqualFrequency dec) s0 
         putStr $ show t
         --(unit,s') <- runStateT (writeFiles 'b' t) s 
         writeFiles t
         return ()
        'c' -> do 
         let s0 = DataMiningState table Nothing [] []
         let dec = head $ extractFromHeaders ( tableHeaders table ) Decision
         (t,s) <- runStateT (dominantAttributeStarter dec) s0 
         let orderedHeaders = zip (tableHeaders table) [(1::Int)..]
             tColHeaders = (tableHeaders t)
             tColHeadersSorted = sortBy (\x y -> case (lookup x orderedHeaders,lookup y orderedHeaders) of 
                                                (Nothing, Nothing) -> EQ 
                                                (Nothing, _)       -> GT
                                                (_,Nothing)        -> LT
                                                (Just xi,Just yi)  -> compare xi yi) tColHeaders 
             cols = map (\header -> extractColumn' t (fst header)) tColHeadersSorted
             t' = Table tColHeadersSorted (mytranspose cols)
             t'' = performPossibleMerges t' dec      
         putStr $ show t''
         --(unit,s') <- runStateT (writeFiles 'c' t'') s 
         writeFiles t''
         return ()
writeFiles :: Table -> IO ()
writeFiles table = do
  let headers = tableHeaders table 
      atts = extractFromHeaders headers Attribute 
      columns = map (\name -> sort ( nub (getColumnValsNoMabies' table name))) atts 
      attcols = zip atts columns
      
      
  hIntervalFile <- openFile "test.int"  WriteMode
  hPutStrLn hIntervalFile "Attributes and their discretized intervals:\n"
  sequence $ map (\(att,colvals) -> case head colvals of 
                                     (Interval l h) -> do 
                                       hPutStrLn hIntervalFile $ att ++ ":\t" ++ (join (intersperse ", " (map show colvals)))
                                     _  -> return ()) attcols    
  hClose hIntervalFile
  
  hTableFile <- openFile "test.data" WriteMode 
  let letters = join $ map (\(att,ad) -> if ad == Attribute then "a " else "d " ) headers 
      firstRow = "< " ++ letters ++ ">"
      colNames = join (intersperse " " ( map fst headers))
      secondRow = "[ " ++ colNames ++ " ]"
      rows = tableData table 
  hPutStrLn hTableFile firstRow
  hPutStrLn hTableFile secondRow
  sequence $ map (\row -> hPutStrLn hTableFile (strRow' row)) rows 
  hClose hTableFile  
  return ()       
dominantAttributeStarter ::String -> MyStateTMonad Table
dominantAttributeStarter dec = do 
  DataMiningState {..} <- get 
 -- liftIO $ putStrLn "Made it here"
  let headerPairs = tableHeaders tableState
      attributes = extractFromHeaders headerPairs Attribute
      att_PosCuts_min_maxLS = map (\att -> let vals =getColumnValsNoMabies' tableState att
                                               numbers = sort . nub $ toDoubles vals 
                                               min = minimum numbers 
                                               max = maximum numbers
                                               in 
                                           (att, calcPossibleCuts numbers, min, max)) attributes
      domattState = map (\(att,ls,min,max) -> (att, AttInfo [] [] ls min max (length ls))) att_PosCuts_min_maxLS
  put $ DataMiningState {getDomAttState = domattState, ..}
  dominantAttribute dec  
  
calcPossibleCuts :: [Double] -> [Double]
calcPossibleCuts doubls = map (\(x,y) -> ( x + y ) /2 ) (zip doubls (tail doubls))

dominantAttribute :: String -> MyStateTMonad Table 
dominantAttribute dec = do 
  DataMiningState {..} <- get
  liftIO ( do 
    putStrLn $"DomAttState: \n" ++ (show getDomAttState) ++ "\n"
    )
  let fixThisTable = case discretizingTable of 
                        Just tf -> tf 
                        Nothing -> tableState
                        
  let headerPairs = tableHeaders fixThisTable 
      rows = tableData fixThisTable 
      attributes = extractFromHeaders headerPairs Attribute
      partitionsLS = map (\at -> compPartG fixThisTable [at]) attributes
      namedparts = map (\attparts -> map (\part -> map (\i ->  getMVal headerPairs dec (findRWithID rows i)) part) attparts) partitionsLS
      attHValPairs = zip attributes (map h namedparts)
  maybebestAttwCutChoices <- getBestAttwCutChoices attHValPairs
  liftIO $ putStrLn $ "Best att: " ++ (show maybebestAttwCutChoices)
  liftIO $ getLine
  --liftIO $ putStrLn "Made it here"
  case maybebestAttwCutChoices of 
   Nothing -> do 
     let str = "ERROR: NO MORE POSSIBLE CUTPOINTS!! Was table consistent to start with?"
     liftIO $ putStrLn str 
     return (case discretizingTable of 
                Nothing -> tableState 
                Just t  -> t)
   Just (att,cuts) -> do 
     hvalues <- sequence $ map (experimentWithCutPoints att dec) cuts
     --liftIO $ putStrLn "Made it here"
     let cutHpairs = zip cuts hvalues
         bestCutPoint =fst $ minimumBy (compare `on` snd) cutHpairs
     liftIO ( do
           putStrLn $ "Best cutPoint for: " ++ att ++ " : " ++ (show bestCutPoint)
           getLine)
     case lookup att getDomAttState of 
            Nothing -> do 
             let str = "this as well should never ever happen."
             liftIO $ putStrLn str 
             return (case discretizingTable of 
                        Nothing -> tableState 
                        Just t  -> t)
            Just (AttInfo {..}) -> do 
              let cuts' = sort (bestCutPoint: getCuts)
                  stops = getAttMinimum : cuts' ++ [getAttMaximum]
                  intervals' = zip stops (tail stops)
                  attinfo' = AttInfo { getCuts = cuts', getIntervals = intervals', ..}
                  entry = (att,attinfo')
                  domattState' = entry: (deleteBy (\x y -> (fst x) == (fst y)) entry getDomAttState) 
              liftIO (do 
                 putStrLn $ "This is the new attState I am trying to add: " ++ (show domattState')
                 getLine
                 )
              put $ DataMiningState {getDomAttState = domattState', ..}
              DataMiningState {..} <- get  --needed because I'm using recordSyntax. need to reget stuff 
              t <- constructTableFromDomAttState dec 
              liftIO $ putStrLn $ "Made it here: " ++ (show t) ++ (show (isConsistent t dec))
              
              case isConsistent t dec of 
                True -> return t 
                False -> do 
                  liftIO $ putStrLn "Made it here 2 "
                  let t_headers = tableHeaders t 
                      atts = extractFromHeaders t_headers Attribute 
                      partition = compPartG t atts 
                      t_rows = tableData t 
                      decEquivalence = map (\intLS -> map (\i -> getMVal t_headers dec (findRWithID t_rows i)  ) intLS ) partition 
                      partDec = zip partition decEquivalence
                      partDec' = filter (\(p,d) -> (length (nub d)) > 1) partDec 
                      --should def be inconsistent to make it to the False branch!
                      newTableIDs =sort . fst $ head partDec' 
                      --newTableIDs is the first partition that causes inconsistency. Now we create the subtable of those.
                      origHeaders = tableHeaders tableState
                      allOrigAtts = extractFromHeaders origHeaders Attribute
                      rows = map (\i -> (i, map (\att -> getMVal origHeaders att (findRWithID (tableData tableState) i)) (allOrigAtts ++ [dec])) ) newTableIDs
                      subTable = Table origHeaders rows
                  liftIO $ putStrLn $ "subTable: " ++ (show subTable)    
                  put $ DataMiningState { discretizingTable= Just subTable, ..}
                  liftIO $ putStrLn $ "Made it here 3"
                  dominantAttribute dec
                  
constructTableFromDomAttState :: String -> MyStateTMonad Table 
constructTableFromDomAttState dec = do 
  DataMiningState {..} <- get 
  let attributesThatHaveCuts = (map fst $ filter (\(att,AttInfo {..}) -> (length getCuts) > 0) getDomAttState) 
      columns = map (extractColumn' tableState) attributesThatHaveCuts 
      attColLS = zip attributesThatHaveCuts columns 
      columns' = map (\(att,column) -> case lookup att getDomAttState of 
                                           Nothing -> column  
                                           Just (attinfo) -> discretizeColumn column (getIntervals attinfo) ) attColLS 
      decHeaders = [(dec,Decision)]
      accHeaders = zip attributesThatHaveCuts (repeat Attribute)
      headers = accHeaders ++ decHeaders
      rows' = mytranspose (columns' ++ [(extractColumn' tableState dec)]) --don't forget the dec column!
  liftIO $ putStrLn $ "Headers to use: " ++ (show headers) ++ "\n Rows to use: " ++ (show rows')
  return (Table headers rows')
  
experimentWithCutPoints :: String ->String -> Double ->MyStateTMonad Double
experimentWithCutPoints att dec cp = do 
  DataMiningState {..} <- get 
  let curTable = case discretizingTable of 
                    Nothing -> Table [(att,Attribute),(dec,Decision)] (let attcol = extractColumn' tableState att 
                                                                           decCol = extractColumn' tableState dec in
                                                                           mytranspose [attcol,decCol]) 
                    Just ddffd -> ddffd
      curTable' = if att `elem` (extractFromHeaders (tableHeaders curTable) Attribute)
                    then curTable  
                    else let origRows = tableData tableState
                             subTableRows = tableData curTable
                             subTableids = map fst subTableRows 
                             headers = tableHeaders tableState
                             newCol = map (\i ->getMVal headers att (findRWithID origRows i)) subTableids
                             subTableHeaders' = (att,Attribute):(tableHeaders curTable)
                             subTableRows' = map (\(mval,(i,ls)) -> (i,mval:ls)) (zip newCol subTableRows)
                             in 
                         Table subTableHeaders' subTableRows' 
      colMVals = extractColumn' curTable' att
      numbers = toDoubles (noMaybies colMVals)
      min = minimum numbers
      max = maximum numbers 
      discretizedCol = discretizeColumn colMVals [(min,cp),(cp,max)]
      experimentalTable = replaceColumns curTable' [(att,discretizedCol)]
      experimentalHeaders = tableHeaders experimentalTable
      attributePartition = compPartG experimentalTable [att] 
      decValPartition =map (\ls -> map (\i -> getMVal experimentalHeaders dec (findRWithID (tableData experimentalTable) i)) ls) attributePartition
  return $ h decValPartition
  
getBestAttwCutChoices :: [(String,Double)] -> MyStateTMonad (Maybe (String,[Double]))
getBestAttwCutChoices [] = return Nothing 
getBestAttwCutChoices attHValPairs = do
  DataMiningState {..} <- get 
  let bestPair =  minimumBy (compare `on` snd) attHValPairs
      posscuts = case lookup (fst bestPair) getDomAttState of 
                   Nothing -> []
                   Just AttInfo {..} -> getPossibleCuts \\ getCuts
  case posscuts of 
    [] -> getBestAttwCutChoices (delete bestPair attHValPairs)
    _  -> return (Just (fst bestPair, posscuts))
    
domAttWithCut :: String -> String ->[Double] -> Double ->Int -> MyStateTMonad Double 
domAttWithCut att dec uniquesSorted cp maxCuts = do
  DataMiningState {..} <- get
  let origTable = tableState     
      table =case (discretizingTable) of 
               Nothing -> origTable 
               Just t  -> t
      kstate = getDomAttState
      



  return cp       
   
          
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
 let attributes = extractFromHeaders (tableHeaders table) Attribute               
 let prs = map (\att -> (att, m (length (tableData table)) (map (\is -> (map (\i -> getMVal (tableHeaders table) dec (getRow table i)) is))
                                                            (compPartG table [att])))) 
                                                              attributes 
     --prs = filter (\(att,mvalue) -> case  prs 
     best = maximumBy (compare `on` snd) prs 
 liftIO $ putStrLn (show prs)     
 liftIO $ putStrLn $ "Worst: " ++ (show best) 
 liftIO $ getLine
 t' <- discretizeTablekp1 meth [fst best] --this updates the kstate as well.
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
  DataMiningState {..} <- get
  let origtable = tableState
  let table = case discretizingTable of 
                Nothing -> origtable
                Just q  -> q 
                
  let attributes = map fst attkLS
  let columnValsLS = map (\(a,k) -> ((getColumnValsNoMabies' origtable) a,k)) attkLS
  let columnMValsLS = map (extractColumn' origtable) attributes
      allColumnNames = map fst (tableHeaders table)
     
      -- : [ [(Int,Double)] ] 
      --decisionColumns = map (extractColumn' table) (extractFromHeaders (tableHeaders table) Decision)
   -- oldColumnAndCutPoints :: [([Maybe Value], [(Double,Double)]]    (header,[cutpoints])
      intervalsLS = map (\(als,k) -> (calcIntervals' meth k als)) columnValsLS
      newAttKIntervalsLS = zip attkLS intervalsLS
      oldColumnAndCutPoints =zip columnMValsLS intervalsLS
      newColumns = (map (\(old, cuts) -> discretizeColumn old cuts) oldColumnAndCutPoints)
      newColsWithNames = zip attributes newColumns
      cols = map (\att -> case lookup att newColsWithNames of 
                            Nothing -> extractColumn' table att 
                            Just v  -> v ) allColumnNames
      rows = mytranspose cols
      table' = replaceRows table rows
      kInfo = discretizingState
      ksub = filter (\(att,_) -> not(att `elem` attributes)) kInfo 
      newAdditions = map (\((att,k),intervals) -> (att, (k,intervals))) newAttKIntervalsLS
      kInfo' = newAdditions ++ ksub 
  liftIO $ putStrLn $ "new intervals: " ++ (show intervalsLS)
  put $ DataMiningState { tableState = origtable, discretizingTable = (Just table'), discretizingState= kInfo', ..}
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

toDoubles :: [Value] -> [Double]
toDoubles ((StrVal _):_) = []
toDoubles ((BoolVal _):_) = []
toDoubles ls = foldr (\x acc -> case x of 
                                       (DoubleVal d) -> d : acc  
                                       (IntVal i   ) -> (fromIntegral i):acc ) [] ls

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