{-#LANGUAGE ScopedTypeVariables, OverloadedStrings, RecordWildCards #-}
module Main where

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
import Text.Printf
import qualified Data.Map as M
import qualified Data.Set as S
hw1 = "hw1datLERS.txt"

main :: IO ()
main = do
  eitherTable <- askAndCreateTable
  case eitherTable of 
    Left err -> do
     putStrLn err 
     putStrLn "Exiting"
    Right table -> do 
   --  putStrLn (show table)
     putStrLn "Your file is acceptable." 
     mmethod <- chooseDiscMethod
     case mmethod of 
      Nothing -> do
       putStrLn "Goodbye!"
       return ()
      Just m -> do
       --putStrLn $ "Method: " ++ (show m)
       let t2 = tableToTable2 table
       let (_, decHeaders) = M.partition (\(_,y) -> y == Attribute) (tableHeaders2 t2)
           decHeader = (snd (M.elemAt 0 decHeaders) )
       let s2 = DataMiningState2 decHeader t2 emptyTable2 M.empty True True 
       let s0 = DataMiningState table Nothing [] False False
       let dec = head $ extractFromHeaders ( tableHeaders table ) Decision         
       case m of 
        'a' -> do 
         (unit,s1) <- runStateT (stateStarter dec) s0 
         liftIO $ putStrLn $ "Beginning equal width discretization.."
         (t,s) <- runStateT (performIntervalDisc EqualWidth dec) s1 
         --(t2',s2') <- runStateT (performIntervalDisc2 EqualWidth) s2
         --case t2' of 
          --  Just t2' -> do  --table is already good to go!
            --  let t = t2Tot t2' 
              --writeFiles t
              --print "done"
              --return () 
            --Nothing ->     
         --print (discretizingTable2 s2')
         --putStrLn "\n\ndone~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"
         --putStr $ show t
         --(unit,s') <- runStateT (writeFiles 'a' t) s 
         let t2 = tableToTable2 t 
         --t2merged <- performPossibleMerges2 t2 decHeader
         --liftIO $ putStrLn "merging complete"
         writeFiles (t2Tot t2 )  --t
         return ()
        'b' -> do 
         (unit,s1) <- runStateT (stateStarter dec) s0 
         liftIO $ putStrLn $ "Beginning equal frequency discretization.."
         (t,s) <- runStateT (performIntervalDisc EqualFrequency dec) s1 
         --putStr $ show t
         --(unit,s') <- runStateT (writeFiles 'b' t) s 
         writeFiles t
         return ()
        'c' -> do 
         (unit,s1) <- runStateT (stateStarter dec) s0 
         liftIO $ putStrLn $ "Beginning dominant attribute discretization.."
         (t,s) <- runStateT (dominantAttribute dec) s1 
         let orderedHeaders = zip (tableHeaders table) [(1::Int)..]
             tColHeaders = (tableHeaders t)
             tColHeadersSorted = sortBy (\x y -> case (lookup x orderedHeaders,lookup y orderedHeaders) of 
                                                (Nothing, Nothing) -> EQ 
                                                (Nothing, _)       -> GT
                                                (_,Nothing)        -> LT
                                                (Just xi,Just yi)  -> compare xi yi) tColHeaders 
             cols = map (\header -> extractColumn' t (fst header)) tColHeadersSorted
             t' = Table tColHeadersSorted (mytranspose cols)
         t'' <- performPossibleMerges' t' dec      
         --putStr $ show t''
         --(unit,s') <- runStateT (writeFiles 'c' t'') s 
         writeFiles t''
         return ()
     putStrLn "Complete!"

  
stateStarter ::String -> MyStateTMonad ()
stateStarter dec = do 
  DataMiningState {..} <- get 
 -- liftIO $ putStrLn "Made it here"
  let headerPairs = tableHeaders tableState
      attributes = extractFromHeaders headerPairs Attribute
      att_PosCuts_min_maxLS = map (\att -> let vals =getColumnValsNoMabies' tableState att
                                               numbers = sort . nub $ toFloats vals 
                                               min = minimum numbers 
                                               max = maximum numbers
                                               in 
                                           (att, calcPossibleCuts numbers, min, max)) attributes
      domattState = map (\(att,ls,min,max) -> (att, AttInfo [] [] ls min max (length ls))) att_PosCuts_min_maxLS
  put $ DataMiningState {getDomAttState = domattState, ..}
  return () 
  
calcPossibleCuts :: [Float] -> [Float]
calcPossibleCuts doubls = map (\(x,y) -> ( x + y ) /2 ) (zip doubls (tail doubls))

dominantAttribute :: String -> MyStateTMonad Table 
dominantAttribute dec = do 
  DataMiningState {..} <- get
  liftIO ( do 
    if verbose then putStrLn $"DomAttState: \n" ++ (show getDomAttState) ++ "\n"
               else return ()
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
  liftIO (do 
           if verbose then putStrLn $ "Best att: " ++ (show maybebestAttwCutChoices)
                      else return ()
           if stops then getLine else (return [])           
           )
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
     liftIO (do 
           if verbose then putStrLn $ "Best cutPoint for: " ++ att ++ " : " ++ (show bestCutPoint)
                      else return ()
           if stops then getLine else (return [])           
           )
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
              DataMiningState {..} <- get 
              liftIO (do 
                    if verbose then putStrLn $ "This is the new attState I am trying to add: " ++ (show domattState')
                               else return ()
                    if stops then getLine else (return [])           
                    )
              put ( DataMiningState {getDomAttState = domattState', ..})
              DataMiningState {..} <- get  --needed because I'm using recordSyntax. need to reget stuff 
              t <- constructTableFromDomAttState dec 
              if verbose then liftIO $ putStrLn $ "Made it here: " ++ (show t) ++ (show (isConsistent t dec))
                         else return ()
              case isConsistent t dec of 
                True -> return t 
                False -> do 
                  --liftIO $ putStrLn "Made it here 2 "
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
                  if verbose then liftIO $ putStrLn $ "subTable: " ++ (show subTable)    
                             else return ()
                  put $ DataMiningState { discretizingTable= Just subTable, ..}
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
  if verbose then liftIO $ putStrLn $ "Headers to use: " ++ (show headers) ++ "\n Rows to use: " ++ (show rows')
             else return ()
  return (Table headers rows')
  
experimentWithCutPoints :: String ->String -> Float ->MyStateTMonad Float
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
      numbers = toFloats (noMaybies colMVals)
      min = minimum numbers
      max = maximum numbers 
      discretizedCol = discretizeColumn colMVals [(min,cp),(cp,max)]
      experimentalTable = replaceColumns curTable' [(att,discretizedCol)]
      experimentalHeaders = tableHeaders experimentalTable
      attributePartition = compPartG experimentalTable [att] 
      decValPartition =map (\ls -> map (\i -> getMVal experimentalHeaders dec (findRWithID (tableData experimentalTable) i)) ls) attributePartition
  return $ h decValPartition
  
getBestAttwCutChoices :: [(String,Float)] -> MyStateTMonad (Maybe (String,[Float]))
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
    
domAttWithCut :: String -> String ->[Float] -> Float ->Int -> MyStateTMonad Float 
domAttWithCut att dec uniquesSorted cp maxCuts = do
  DataMiningState {..} <- get
  let origTable = tableState     
      table =case (discretizingTable) of 
               Nothing -> origTable 
               Just t  -> t
      kstate = getDomAttState
      



  return cp       
   
performIntervalDisc2 :: IntervalMethod-> MyStateTMonad2 (Maybe Table2)
performIntervalDisc2 meth = do 
        initializeDiscretization meth 
        s <- get 
        let t2 =(discretizingTable2 s)
            dec = (stateDec s)
            (x,y) = l2 t2 dec
            
        if x == y 
          then do 
            liftIO $ print "Table consistent after initial 2 cutpoints. Attempting merges:"
            t2' <- liftIO $ performPossibleMerges2 t2 dec 
            let t2'' = dropSillyColumns2 t2'
            return (Just t2'')
          else do 
            return Nothing 
continueDiscretizing2 ::IntervalMethod -> MyStateTMonad2 () 
continueDiscretizing2 meth = do 
  DataMiningState2 {..} <- get 
  return ()
initializeDiscretization :: IntervalMethod -> MyStateTMonad2 () -- the table is in the state. no need to pass 
initializeDiscretization meth = do 
  s <- get 
  let (Table2 {..}) = tableState2 s 
  let (attMap,decMap) = M.partition (\(_,ad) -> ad == Attribute) tableHeaders2
      origHeaders = tableHeaders2 
  let ihattStateMap = M.map (\h -> let nubvals = tableNubColumns M.! h 
                                       nubfloats = S.map toFloat nubvals 
                                       ((minn,maxx),cutpoints) = cutphelper nubfloats
                                       maxcuts = S.size cutpoints
                                       in 
                                   (h,AttInfo2 S.empty S.empty cutpoints minn maxx maxcuts)) attMap 
      discState = M.fromList (M.elems ihattStateMap)
  DataMiningState2 {..} <- get 
  let Table2 {..} = discretizingTable2
  let discT = Table2 {tableHeaders2 = origHeaders, ..}
  DataMiningState2 {..} <- get 
  put $ DataMiningState2 { discretizingState2=discState, discretizingTable2 = discT, ..}
  
  x <- sequence $ M.elems $ M.map (\h -> addaCutPointTo h meth) attMap 
  -- x should be a list of True. 
  --now the discretizingTable has 2 intervals for every column 
  --liftIO $ print x 
  s <- get 
  --liftIO $ print s 
  return ()  -- $ tableState2 s 
addaCutPointTo :: Header -> IntervalMethod-> MyStateTMonad2 Bool 
addaCutPointTo h meth = do 
  DataMiningState2 {..} <- get 
  let origColumns = tableColumns2 tableState2
      origCol =  (origColumns) M.! h 
      attinfo = discretizingState2 M.! h 
      curCuts = getCuts2 attinfo 
      maxCuts = getMaxCuts2 attinfo
      possCuts = getPossibleCuts2 attinfo 
      cutPool = (possCuts) S.\\ curCuts 
  if S.null cutPool
   then do 
     return False     
   else do
     let nubvals = (tableNubColumns tableState2) M.! h 
         --nubfloats = S.map toFloat nubvals 
         roughcuts = case meth of 
                       EqualWidth -> (calcRoughEqualWidthCuts2 ((S.size curCuts) + 2) nubvals)
                                    
         min = getAttMinimum2 --attinfo
         max = getAttMaximum2 --attinfo
         roughcutsclean = S.map (\x -> lookupClosest' x possCuts) roughcuts
         ls = S.insert max roughcutsclean
         (_,intervals) = S.foldl (\(prev,s) cur -> (cur, S.insert (prev,cur) s) ) (min, S.empty) ls 
         
         AttInfo2 {..} = attinfo 
         attinfo' = AttInfo2 { getCuts2 = roughcutsclean, getIntervals2 = intervals,..}
         newcol = discretizeColumn2 origCol intervals  
         f _ = Just newcol 
         f2 _ = Just attinfo'
         --newColumns = M.alter f h origColumns
         
      
     --s <- get -- I have no idea if I need this I just put it here.
     
     let Table2 {..} = discretizingTable2 
     let discT2' = (Table2 { tableColumns2 = M.alter f h tableColumns2, .. })
    -- liftIO $ putStrLn $ "New discTable: " ++ (show discT2')
     DataMiningState2 {..} <- get 
     put $ DataMiningState2 {discretizingTable2 = (discT2'), discretizingState2= M.alter f2 h discretizingState2, ..}
     return True 
     {-
obeyColumns :: Table2 -> Table2 
obeyColumns t2 = let Table2 {..} = t2 
                     nubs'= M.map (S.fromList . M.elems) tableColumns2
                     data2' = M.foldrWithKey  (\h columnmap acc -> M.mapWithKey (\i v -> let f Nothing = Just (M.singleton h v) 
                                                                                             f (Just m) = let f2 _ = Just v in Just (M.alter f2 h m) 
                                                                                           in 
                                                                                         M.alter f i acc ) columnmap ) tableData2 tableColumns2    -- :: Map Int (Map Header Value)
                     in 
                 Table2 { tableData2 = data2', tableNubColumns = nubs', ..}
                                                                                             --4 hardest lines of code in my entire life. 

    -}
cutphelper :: S.Set Float -> ((Float,Float),S.Set Float) 
cutphelper s = let (minn,nubfloats') = S.deleteFindMin s
                   (maxx,cutpoints)  = S.foldl (\(prev,s') x ->(x,S.insert ((prev + x) / 2) s') ) (minn, S.empty) nubfloats' 
                   in 
               ((minn,maxx),cutpoints)
performIntervalDisc :: IntervalMethod -> String -> MyStateTMonad Table 
performIntervalDisc meth dec = do 
  s <- get
  --liftIO $ putStrLn "eifjeifjeifjeifjeifeifj"
  let table = tableState s 
      headerPairs = tableHeaders table
      attributes = extractFromHeaders headerPairs Attribute
  t' <- discretizeTable meth (zip attributes (repeat 1))
  s' <- get 
  if verbose s' then liftIO $ putStrLn $ show ( discretizingTable s)
             else return () 
  let (x,y) = l t' dec 
  if verbose s' then liftIO $ putStrLn $ "First L: " ++ (show (x,y))
             else return ()
  --liftIO $ putStrLn $ "hellloooooooooooooooo"
  if x == y then return t'
   else do 
    --liftIO $ putStrLn $ "about to start continueDiscretizing"
    finalTable <- continueDiscretizing meth dec 
    if verbose s' then liftIO $ putStrLn $ "table before merging:\n" ++ (show finalTable)
               else return ()
    liftIO $ putStrLn $ "merging.."           
    --t2 <- liftIO $ performPossibleMerges2 (tableToTable2 finalTable) (dec, Decision) 
    finalTable' <- liftIO $ performPossibleMerges' finalTable dec  
    if verbose s' then liftIO $ putStrLn $ "table AFTER merging:\n" ++ (show finalTable')
               else return ()
    return finalTable'
    
continueDiscretizing :: IntervalMethod -> String -> MyStateTMonad Table
continueDiscretizing meth dec = do  
 --liftIO $ putStrLn "continueDiscretizing"
 mbest <- maybeGetBest dec
 case mbest of 
    Nothing -> do 
      let str = "Error!! No more cut points left to split any attribute!!"
      liftIO $ putStrLn str 
      s <- get 
      return (case discretizingTable s of 
                    Nothing -> tableState s
                    Just t  -> t)
    (Just best) -> do 
      s <- get 
  
     
      if verbose s then liftIO $ putStrLn $ "Worst: " ++ (show best) 
                 else return ()
      if stops s then liftIO $ getLine
               else return []
      t' <- discretizeTablekp1 meth [best] --this updates the kstate as well.
      --DataMiningState {..} <- get
      let (x,y) = l t' dec 
      if verbose s then liftIO $ putStrLn (show (x,y))
        else return ()
      if stops s then liftIO $ getLine
        else return []
      let p =  round (((fromIntegral x) / (fromIntegral y)) * 100) 
      fff <- liftIO $ putStrLn $ "Percent consistent: " ++ (show p) ++ "%"
      liftIO $ hFlush stdout
      case fff of 
         () -> return ()
      if x == y then return t'
                else continueDiscretizing meth dec 

maybeGetBest :: String -> MyStateTMonad (Maybe String)
maybeGetBest dec = do 
 s <- get 
 let table = case discretizingTable s of 
                  Nothing -> tableState s 
                  Just q  -> q 
 if verbose s then liftIO $ putStrLn $ "Here is the AttState: " ++ (show (getDomAttState s))
    else return ()
 let attributes =map fst $ filter (\(att,attinfo) -> (length (getCuts attinfo)) < (getMaxCuts attinfo))(getDomAttState s) 
 --extractFromHeaders (tableHeaders table) Attribute 
 case attributes of 
    [] -> return Nothing
    _  -> do 
      let prs = map (\att -> (att, m (length (tableData table)) (map (\is -> (map (\i -> getMVal (tableHeaders table) dec (getRow table i)) is))
                                                            (compPartG table [att])))) 
                                                              attributes 
          prs' = nubBy (\a b -> (snd a) == (snd b)) prs 
     --prs = filter (\(att,mvalue) -> case  prs 
          best = fst $ maximumBy (compare `on` snd) prs'           
      if verbose s then liftIO $ putStrLn $ "Pairs: " ++ (show prs) 
        else return ()
      return (Just best)     
 
{- 
maybeGetBest2 :: Header -> MyStateTMonad2 (Maybe Header)
maybeGetBest2 dec = do 
 s <- get 
 let table = discretizingTable2 s 
 if verbose2 s then liftIO $ putStrLn $ "Here is the AttState: " ++ (show (discretizingState2 s))
    else return ()
 let attributes = (S.fromList . M.elems) $ M.filter (\attinfo -> (S.size (getCuts2 attinfo)) < (getMaxCuts2 attinfo))(discretizingState2 s) 
 --extractFromHeaders (tableHeaders table) Attribute 
 case S.null attributes of 
    True -> return Nothing
    _  -> do 
      let columns = tableColumns2 table 
          len = (M.size . snd) (M.elemAt 0 columns)
          f = (\x -> compPartG2 table (S.singleton x))
      let prs = S.foldr (\att acc -> let f _ = Just att in 
                                       M.alter f (m (len) (S.map (\is -> (S.map (\i ->  (columns M.! dec) M.! i )-- getMVal (tableHeaders table) dec (getRow table i)) is))
                                                                                (f att) ))) ) acc) M.empty attributes
{-                                                              
      let prs = map (\att -> (att, m (length (tableData table)) (map (\is -> (map (\i -> getMVal (tableHeaders table) dec (getRow table i)) is))
                                                            (compPartG table [att])))) 
                                                              attributes 
-}  
      let best = (snd . M.findMax) prs 
      if verbose s then liftIO $ putStrLn $ "Pairs: " ++ (show prs) 
        else return ()
      return (Just best)      
  -}         
discretizeTablekp1 :: IntervalMethod -> [(String)] -> MyStateTMonad Table
discretizeTablekp1 meth ls = discretizeTable meth (zip ls (repeat 1))  
  
discretizeTable ::IntervalMethod -> [(String,Int)] -> MyStateTMonad Table  
discretizeTable meth attkLS  = do 
  --liftIO $ putStrLn "In descretizeTable.."
  DataMiningState {..} <- get
  let origtable = tableState
  let table = case discretizingTable of 
                Nothing -> origtable
                Just q  -> q 
  --liftIO $ putStrLn "In descretizeTable a little further.."              
  let attkLS' = foldr (\(a,k) acc -> case lookup a getDomAttState of 
                                        Nothing -> acc 
                                        Just (AttInfo {..}) -> let k' = (length getCuts) + k + 1 in 
                                          if (k' -1) <= getMaxCuts
                                            then (a,k'):acc 
                                            else acc) [] attkLS
  
  {-filter (\(a,k) -> case lookup a getDomAttState of 
                                    Nothing -> False
                                    Just (AttInfo {..}) -> ((length getCuts) + k) <= getMaxCuts ) attkLS
 -}   
      attributes = map fst attkLS'
  if verbose then liftIO $ putStrLn $ "Here are the atts I am about to discretize: " ++ (show attkLS')
    else return ()
  --liftIO $ putStrLn "In descretizeTable a little further about to get noMaybies.."  
  let columnValsLS = map (\(a,k) -> (getColumnValsNoMabies' origtable a,k)) attkLS'
  let columnMValsLS = map (extractColumn' origtable) attributes
      allColumnNames = map fst (tableHeaders table)
     
      -- : [ [(Int,Float)] ] 
      --decisionColumns = map (extractColumn' table) (extractFromHeaders (tableHeaders table) Decision)
   -- oldColumnAndCutPoints :: [([Maybe Value], [(Float,Float)]]    (header,[cutpoints])
      intervalsLS = map (\(als,k) -> (calcIntervals' meth k als)) columnValsLS
      newAttKIntervalsLS = map (\((a,k),intervals) -> (a,(k,intervals))) (zip attkLS' intervalsLS)
      oldColumnAndCutPoints =zip columnMValsLS intervalsLS
      newColumns = (map (\(old, cuts) -> discretizeColumn old cuts) oldColumnAndCutPoints)
      newColsWithNames = zip attributes newColumns
      cols = map (\att -> case lookup att newColsWithNames of 
                            Nothing -> extractColumn' table att 
                            Just v  -> v ) allColumnNames
      rows = mytranspose cols
      table' = replaceRows table rows
      kInfo = getDomAttState --discretizingState
      kInfo' = foldr (\e@(a, AttInfo {..}) acc -> case lookup a newAttKIntervalsLS of 
                                                Nothing -> e:acc
                                                Just (k,intervals) -> (a,AttInfo {getCuts =((tail . init) (sort(nub(foldr (\(x,y) acc -> x:y:acc) [] intervals)))), getIntervals = intervals,..}):acc ) [] kInfo
      --ksub = filter (\(att,_) -> not(att `elem` attributes)) kInfo 
      --newAdditions = map (\((att,k),intervals) -> (att, (k,intervals))) newAttKIntervalsLS
      --kInfo' = newAdditions ++ ksub 
  if verbose then liftIO (do 
                            putStrLn $ "new intervals: " ++ (show (length intervalsLS))
                            putStrLn (show table'))
    else return ()
  put $ DataMiningState { tableState = origtable, discretizingTable = (Just table'), getDomAttState= kInfo', ..}
  if stops then liftIO $ getLine
    else return []
  return table'
  
discretizeColumn :: [Maybe Value] -> [(Float,Float)] -> [Maybe Value]  
discretizeColumn [] _ = []
discretizeColumn a@((Just (StrVal _)):_) _  =  a 
discretizeColumn a@((Just (BoolVal _)):_) _ =  a
discretizeColumn column intervals = map (categorize intervals) column 

discretizeColumn2 :: M.Map Int Value ->S.Set (Float,Float) -> M.Map Int Value 
discretizeColumn2 columnMap intervalSet = M.map (\v -> categorize2 v intervalSet) columnMap 

categorize2 :: Value -> S.Set (Float,Float) -> Value 
categorize2 (FloatVal v) s = case S.lookupLE (v,v) s of
                               Nothing -> let (m1,m2) = S.findMax s in Interval m1 m2 
                               Just (m1,m2) -> Interval m1 m2 

categorize :: [(Float,Float)] -> Maybe Value -> Maybe Value 
categorize intervals (Just (FloatVal d))  =   let mInterval = find (\pair -> d >= (fst pair) && d <= (snd pair)) intervals in 
                                        case mInterval of 
                                           Nothing -> Just $ StrVal "ERROR"
                                           Just i  ->Just $ Interval (fst i) (snd i)
categorize intervals (Just (IntVal i)) = categorize intervals (Just (FloatVal (fromIntegral i)))
categorize _ a@_ = a 
                                           
calcIntervals :: IntervalMethod -> Int -> [Maybe Value] -> [(Float,Float)]
calcIntervals meth x ls = calcIntervals' meth x (noMaybies ls)

calcIntervals' :: IntervalMethod -> Int -> [Value] -> [(Float,Float)]
calcIntervals' EqualWidth = calcEqualWidthIntervals
calcIntervals' EqualFrequency = calcEqualFrequencyIntervals

toFloats :: [Value] -> [Float]
toFloats ((StrVal _):_) = []
toFloats ((BoolVal _):_) = []
toFloats ls = foldr (\x acc -> (toFloat x):acc ) [] ls

toFloat :: Value -> Float
toFloat (FloatVal d) = d
toFloat (IntVal i)   = fromIntegral i                                      

calcEqualFrequencyIntervals :: Int -> [Value] -> [(Float,Float)]
calcEqualFrequencyIntervals _ ((StrVal _):_) = []
calcEqualFrequencyIntervals _ ((BoolVal _):_) = []
calcEqualFrequencyIntervals n ls = let ls' = foldr (\x acc -> case x of 
                                                               (FloatVal d) -> d : acc  
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
calcEqualWidthIntervals' :: Int -> [Maybe Value] -> [(Float,Float)]
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


calcRoughEqualWidthCuts2 :: Int -> S.Set Value -> S.Set Float
calcRoughEqualWidthCuts2 numintervals valSet = do 
  if S.null valSet then S.empty
  else do
    let mv = (case (S.elemAt 0 valSet) of 
         (StrVal _)     -> Nothing 
         (BoolVal _)    -> Nothing
         (IntVal i)     -> (Just (fromIntegral i))
         (FloatVal d)  -> Just d)
    case mv of 
      Nothing -> S.empty
      Just v  -> do 
         let max = case S.findMax valSet of 
                     IntVal i -> fromIntegral i
                     FloatVal d -> d 
             min = case S.findMin valSet of
                     IntVal i -> fromIntegral i 
                     FloatVal d -> d              
         let width = (max - min)/((fromIntegral numintervals))
         S.fromList $ map (\i -> ((min + (fromIntegral i))*width)) [1..(numintervals-1)]
         --S.fromList $ map (\(x,y) -> ((min + (fromIntegral x)*width),(min + (fromIntegral y)*width))) (zip [0..numintervals] [1..numintervals])


calcEqualWidthIntervals :: Int -> [Value] -> [(Float,Float)]
calcEqualWidthIntervals numintervals columnvals = do  
  if length columnvals == 0 then []
  else do
     let mv = (case head columnvals of 
               (StrVal _)     -> Nothing 
               (BoolVal _)    -> Nothing
               (IntVal i)     -> (Just (fromIntegral i))
               (FloatVal d)  -> Just d)
     case mv of 
      Nothing -> []
      Just v  -> do 
         let max = case maximum columnvals of 
                     IntVal i -> fromIntegral i
                     FloatVal d -> d 
             min = case minimum columnvals of
                     IntVal i -> fromIntegral i 
                     FloatVal d -> d              
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

writeFiles :: Table -> IO ()
writeFiles table = do
  let headers = tableHeaders table 
      atts = extractFromHeaders headers Attribute 
      columns = map (\name -> sort ( nub (getColumnValsNoMabies' table name))) atts 
      attcols = zip atts columns
      
  putStrLn $ "Writing Interval Info.."    
  hIntervalFile <- openFile "test.int"  WriteMode
  hPutStrLn hIntervalFile "Attributes and their discretized intervals:\n"
  sequence $ map (\(att,colvals) -> case head colvals of 
                                     (Interval l h) -> do 
                                       hPutStrLn hIntervalFile $ att ++ ":\t" ++ (join (intersperse ", " (map show colvals)))
                                     _  -> return ()) attcols    
  hClose hIntervalFile
  
  putStrLn $ "Writing table file.."
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
    
    
    
slow_fib :: Int -> Integer
slow_fib 0 = 0
slow_fib 1 = 1
slow_fib n = (memoized_fib (n-2)) + (memoized_fib (n-1))

memoized_fib :: Int -> Integer
memoized_fib = memoize slow_fib 