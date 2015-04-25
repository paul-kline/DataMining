{-#LANGUAGE RecordWildCards #-}
module DataMiningTools where

import DataMiningTypes
import TableMaker
import Data.List
import Prelude hiding (id)
import Control.Monad
import Control.Monad.State
import Data.Text (pack, unpack)
import Text.Read (readMaybe)
import Control.Monad
import System.IO.Unsafe
import Data.Function.Memoize
import qualified Data.Map as M
import qualified Data.Set as S
runEvaluate ::  String ->IO (Either String (String, DataMiningState))
runEvaluate fileLoc= do
              eitherTable <- readInTable fileLoc  
              case eitherTable of
                (Left err)    -> do 
                  putStrLn err
                  return (Left err)
                (Right table) -> do
                  let s0 =  DataMiningState table Nothing [] False False              
                  result <- runStateT (evaluate) s0
                  return $ Right result

runEvaluate' :: Table ->IO (String , DataMiningState)
runEvaluate' table = do
   let s0 = DataMiningState table Nothing [] False False
   runStateT (evaluate) s0

evaluate :: MyStateTMonad String
evaluate = do
  consistencyofData <- checkTableConsistency 
  case consistencyofData of
    False -> return "inconsistent data"
    True -> do
      lem1rules <- lem1
      s <- get
      let table = tableState s 
      let rulesAndCoverings = zip lem1rules (map (ruleCovers table) lem1rules)
      liftIO $ sequence $ map (putStrLn . show) rulesAndCoverings
      liftIO $ putStrLn "LEM2 BEGIN~~~~~~~~~~~~~~~~~~~"
      lem2rules <-lem2
      
      return "nothing"
  return "hello"

lem1 :: MyStateTMonad [Rule]
lem1 = do
        s <- get
        let table = tableState s
        let headerPairs = tableHeaders table
        let decisions = extractFromHeaders headerPairs Decision
        -- :: [[MaybeValues]]
        --x <- sequence (map getUniqueValuesFor decisions) --unique vals for each decision column
        --decision, domain pairs
        --let decDomPairs = zip decisions x 
        xss <- sequence $ map computeLEM1RulesForDecisionColumm decisions
        return $ join xss 
lem2 :: MyStateTMonad [Rule]
lem2 = do
        s <- get
        let table = tableState s 
        attValLS <- getAllAttributeValuePairs
        let attValBlocks = map (\av -> attribValPairsCover table [av]) attValLS
        let lem2Table = map (\(av,block) -> LEM2Row av block []) (zip attValLS attValBlocks)
        let headerPairs = tableHeaders table
        let decisions =  extractFromHeaders headerPairs Decision
        x <- sequence $ map getAllAttributeValuePairsForAtt decisions
        let decValPairs = join x
        let bigGoals = map head (map (\(d,v) -> decisionPart table d v) decValPairs)
        --ruleslsls <- sequence $ map (\g -> computeLEM2WithTableGoal lem2Table g g) bigGoals
        
        liftIO $ putStrLn $ show (head lem2Table)
        return []
        
        
       
getAllAttributeValuePairs :: MyStateTMonad [(String,Value)]
getAllAttributeValuePairs = do
   s <- get 
   let table = tableState s
   let headerPairs = tableHeaders table 
   let allAtts = extractFromHeaders headerPairs Attribute  
   lls <- sequence ( map getAllAttributeValuePairsForAtt allAtts )
   return $ join lls
   
getAllAttributeValuePairsForAtt ::String -> MyStateTMonad [(String,Value)]
getAllAttributeValuePairsForAtt str = do 
  u <- getUniqueValuesFor str 
  return (zip (repeat str) (noMaybies u))
  
computeLEM1RulesForDecisionColumm :: String -> MyStateTMonad [Rule]
--computeLEM1RulesForDecisionColumm decCol = []        
computeLEM1RulesForDecisionColumm decCol = do
                                s <- get
                                let table = tableState s
                                uniqueValsForColumn <- getUniqueValuesFor decCol
                                let noMaybiesVals = noMaybies uniqueValsForColumn
                                xss <- sequence $ map (computeLEM1RulesForDecValue decCol) noMaybiesVals 
                                return $ join xss 
noMaybies :: [Maybe a] -> [a]
noMaybies [] = []
noMaybies (x:xs) = case x of
                     Nothing -> noMaybies xs
                     Just v  -> v: (noMaybies xs)


                                                     
computeLEM1RulesForDecValue :: String -> Value -> MyStateTMonad [Rule]
computeLEM1RulesForDecValue decCol val = do
                                    s <- get
                                    let table = tableState s
                                    let headerPairs = tableHeaders table 
                                    let decPar = decisionPart table decCol val 
                                    let goodPart = head decPar
                                    let allAtts = extractFromHeaders headerPairs Attribute  
                                    let globalCovering = findGlobalCoverFor table decPar allAtts 
                                    liftIO $ putStrLn $ "GLOBAL COVERING: " ++ (show globalCovering)
                                    case globalCovering of 
                                        Just gc -> computeLem1RulesThatCover goodPart gc decCol val
                                        Nothing -> do 
                                                     liftIO $ putStrLn ("no freaking global covering for: (" ++ (show decCol) ++ ", " ++ (show val) )
                                                     return []
                                     
                                          
findGlobalCoverFor :: Table -> [[Int]] -> [String] -> Maybe [String]
findGlobalCoverFor _ _ [] = Nothing
findGlobalCoverFor table decPar atts = case (compPartG table atts) <== decPar of
                                            True  -> Just (dropAttsICan' table decPar atts 0)
                                            False -> Nothing
                                           
computeLem1RulesThatCover :: [Int]-> [String] -> String -> Value -> MyStateTMonad [Rule]
computeLem1RulesThatCover [] _ _ _= return []
computeLem1RulesThatCover leftToCover@(x:xs) attributes decCol decVal= do
  s <- get
  let table = tableState s
  let headerPairs = tableHeaders table
  let rows = tableData table              
  let row1 =  findRWithID rows x  
  let rule = mkRuleFromAttsAndRow headerPairs attributes row1 decCol
  rule' <- reduceRule rule
  let covered = ruleCovers table rule'
  let leftToCover' = leftToCover `setMinus` covered
  moreRules <- computeLem1RulesThatCover leftToCover' attributes decCol decVal 
  return (rule' : moreRules)
  
  
setMinus ::(Eq a) => [a] -> [a] -> [a]
setMinus [] _ = []
setMinus xs [] = xs
setMinus (x:xs) ms = if x `elem` ms then setMinus xs ms 
                                    else x : (setMinus xs ms)
reduceRule :: Rule -> MyStateTMonad Rule
reduceRule r@(Rule (x:[]) p) = return r --one attribute, can't reduce 
reduceRule r@(Rule avps (decCol,decVal)) = do
                                        s <- get
                                        let table = tableState s 
                                        let headers = tableHeaders table 
                                        --let covered = ruleCovers table r 
                                        let decPar = decisionPart table decCol decVal
                                        let goodPart = head decPar
                                        let avps' = dropAttsICan table goodPart avps 0  
                                        return $ Rule avps' (decCol, decVal)
dropAttsICan' :: Table -> [[Int]] -> [String] -> Int -> [String]
dropAttsICan' table part atts i | i >= (length atts) = atts -- means we're been through them all 
                                | otherwise = let withoutI = filter (/= (atts !! i)) atts in
                                                case (compPartG table withoutI) <== part of
                                                    True -> dropAttsICan' table part withoutI i 
                                                    False -> dropAttsICan' table part atts (i + 1)
                                                    
dropAttsICan :: Table -> [Int] -> [(String,Value)] -> Int -> [(String,Value)]       
dropAttsICan table goodPart atts i | i >= (length atts) = atts
                                   | otherwise = let withoutI = filter (/= (atts !! i)) atts in
                                                 let covering = attribValPairsCover table withoutI in 
                                                   if covering `isSubSet` goodPart 
                                                     then dropAttsICan table goodPart withoutI i --then it's totally cool to be rid of it 
                                                    else  dropAttsICan table goodPart atts (i+1)  -- need attribute 
                                                    
extractColumn :: String -> MyStateTMonad [Maybe Value]
extractColumn header = do 
  s <- get 
  let table = tableState s 
  return $ extractColumn' table header 
  
extractColumn' :: Table -> String -> [Maybe Value]
extractColumn' table header = let headerPairs = tableHeaders table in
  let rows = tableData table in
  map (getMVal headerPairs header) rows  

extractColumn2' :: Table2 -> Header -> M.Map Int Value
extractColumn2' t2 header = (tableColumns2 t2) M.! header 

extractColumn2Nubs' :: Table2 -> Header -> S.Set Value 
extractColumn2Nubs' t2 header = (tableNubColumns t2) M.! header
  
getColumnValsNoMabies :: String -> MyStateTMonad [Value]
getColumnValsNoMabies header = do
  s <- get 
  let table = tableState s
  return $ getColumnValsNoMabies' table header 
                                
getColumnValsNoMabies' :: Table -> String -> [Value]
getColumnValsNoMabies' table header = 
  let headerPairs = tableHeaders table 
      rows = tableData table   in
  foldr (\r acc -> case getMVal headerPairs header r of 
                            Nothing -> acc
                            Just v  -> v : acc) [] rows                                  

                            
getUniqueValuesFor :: String -> MyStateTMonad [Maybe Value]
getUniqueValuesFor header = do
  s <- get
  let table = tableState s
  let headerPairs = tableHeaders table
  let rows = tableData table
  let allMbVals = map (getMVal headerPairs header ) rows
  return $ removeDuplicates allMbVals
  
removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = if x `elem` xs then removeDuplicates xs
                                         else x : (removeDuplicates xs)
  
checkTableConsistency :: MyStateTMonad Bool
checkTableConsistency = do
    s <- get
    let table = tableState s
    let headerPairs = tableHeaders table 
    let attributes = extractFromHeaders headerPairs Attribute
    let decisions = extractFromHeaders headerPairs Decision
    let consistencies = map (checkDecConsistency table attributes) decisions
    case and consistencies of
      False -> do 
        liftIO (putStrLn ("ERROR: The Table has inconsistent data in the following partitions:\n" ++ (join (map (\(b,d) -> if b then "" else d) (zip consistencies decisions)))))
        return False
      True -> do
        liftIO $ putStrLn "DATA CONSISTENT"
        return True       
    
    
checkDecConsistency :: Table -> [String] -> String  -> Bool
checkDecConsistency table attributes decision =((compPartG table attributes) <== (compPartG table [decision]))
    
extractFromHeaders :: [Header] -> AttDec -> [String]
extractFromHeaders [] _ = []
extractFromHeaders (h:hs) attdec = if (snd h) == attdec then (fst h):(extractFromHeaders hs attdec) 
                                                        else extractFromHeaders hs attdec
  


                    
decisionPart :: Table -> String -> Value -> [[Int]]
--decisionPart [] _ _ = []
decisionPart table header val = let part = compPartG table [header] in
                               let mID = findAnIDWithAtt (tableData table) (tableHeaders table) header val in
                                   let interestingPart = findPartWithID mID part in
                                    let nonInterestingPart = joinPartsNotContainingID mID part in
                                        [interestingPart, nonInterestingPart]

findAnIDWithAtt :: [Row] -> [Header] -> String -> Value -> Int         
--findAnIDWithAtt []
findAnIDWithAtt (x:xs) hs att val = if (getMVal hs att x) == (Just val) then id x
                                               else findAnIDWithAtt xs hs att val 

findPartWithID :: Int -> [[Int]] -> [Int]
findPartWithID i (x:xs) = if i `elem` x then x
                                        else findPartWithID i xs
                                        
joinPartsNotContainingID :: Int -> [[Int]] -> [Int]
joinPartsNotContainingID _ [] = []
joinPartsNotContainingID i (x:xs) = if i `elem` x then (joinPartsNotContainingID i xs)
                                                  else x ++ (joinPartsNotContainingID i xs)                    
compPartG2 :: Table2 -> S.Set Header -> S.Set (S.Set Int)
compPartG2 t2 =  compPartG2' (tableColumns2 t2) 

{- let rowMap = tableData2 t2  -- :: M.Map Int (M.Map Header Value)
                            reducedRows = M.map (\m -> M.filterWithKey (\h v ->(h `S.member` headers)) m) rowMap
                            part =S.fromList $ M.elems $ 
                                           M.foldrWithKey (\i m acc -> let f = (\k v -> if k == m then Just (S.insert i v) else Nothing)  
                                                                           (madd,acc') = M.updateLookupWithKey f m acc
                                                                            in
                                                                       case madd of 
                                                                         Just _ -> acc' 
                                                                         Nothing ->M.insert m (S.singleton i) acc) M.empty reducedRows -- ((M.Map Header Value), (S.Set Int))
                            in 
                        part
                        -}
--this method does not rely on the rows!!                        
compPartG2' :: M.Map Header (M.Map Int Value) -> S.Set Header -> S.Set (S.Set Int)
compPartG2' columns headerSet = let numEntries = (M.size . snd) (M.elemAt 0 columns )
                                    x = foldr (\i acc -> let row = S.foldr (\h ac -> S.insert ((columns M.! h) M.! i) ac) S.empty headerSet 
                                                             f Nothing = Just (S.singleton i)
                                                             f (Just s) = Just (S.insert i s) in 
                                                         M.alter f row acc ) M.empty [1..numEntries] in 
                                (S.fromList . M.elems) x
compPartG :: Table -> [String] -> [[Int]]
compPartG table fs = compPartG' table (tableData table) (tableHeaders table) fs []
                                 --extractor
compPartG' :: Table -> [Row] ->[Header] -> [String] -> [[Int]] -> [[Int]]
compPartG' _ [] hs fs curPart = curPart
compPartG' table (x:xs) hs fs curPart = compPartG' table xs hs fs (addAppropriatelyG table x hs fs curPart) 
                             --Extractor
addAppropriatelyG :: Table -> Row ->[Header] -> [String] -> [[Int]] -> [[Int]]
addAppropriatelyG t row hs fs [] = [[id row]]
addAppropriatelyG table row hs fs ((x:xs):xs') = if ( (getValsG hs fs row) == (getValsG hs fs (findRWithID (tableData table) x)) ) then ((id row):x:xs):xs'
                                                                                              else ((x:xs):(addAppropriatelyG table row hs fs xs'))
                  --Extractor                                                                                               
getValsG :: [Header] -> [String] -> Row -> [Maybe Value]
getValsG _ [] r = []
getValsG hs (f:fs) r = (getMVal hs f r):(getValsG hs fs r)

getRow :: Table -> Int -> Row 
getRow table int = findRWithID (tableData table) int

findRWithID :: [Row] -> Int -> Row
findRWithID rows i = case find (\x -> (fst x) == i) rows of 
                        Nothing -> 
                          let x = unsafePerformIO $ putStrLn $ "OH NO!!!1" ++ (show rows) ++ (show i) in
                          case x of 
                            () -> rows !! (i-1) --should throw exception
                        Just r -> r 
 {-                                         
findID :: [Row] -> Int -> Row 
--findID [] i = Nothing
findID (x:xs) i = if i == (id x) then x
                               else findID xs i
-}
lookupClosest :: (Ord a, Num a) => a -> S.Set a -> Maybe a
lookupClosest x set = case (S.lookupGE x set, S.lookupLE x set) of 
                        (Nothing, Just y) -> Just y 
                        (Just y, Nothing) -> Just y 
                        (Nothing, Nothing) -> Nothing
                        (Just y1, Just y2) -> if (abs (y1 - x)) < (abs (y2 - x)) then Just y1 else Just y2
lookupClosest' :: (Ord a, Num a) => a -> S.Set a -> a
lookupClosest' x set = case lookupClosest x set of 
                        Nothing -> x 
                        Just y  -> y 
getMVal :: [Header] -> String -> Row -> Maybe Value
getMVal headers str row  = let mPos = getPosition headers str in
                             case mPos of
                                  Nothing -> Nothing
                                  (Just pos) -> (snd row) !! pos 

getPosition :: [Header] -> String -> Maybe Int 
getPosition [] _ = Nothing
getPosition (x:xs) str = if fst x == str then Just 0 
                                         else case getPosition xs str of
                                                   Nothing -> Nothing
                                                   (Just pos) ->(Just (1 + pos ))  


                                                   
mySort :: (Ord a) => [[a]] -> [[a]]
mySort ls = sort (map sort ls)   

(<==) :: (Ord a) => [[a]] -> [[a]] -> Bool
[]  <== []  = True
_   <== []  = False
ls1 <== ls2 = and $ map (isSubSetOfAnySetIn ls2) ls1

isSubSetOfAnySetIn ::(Ord a) => [[a]] -> [a] -> Bool
isSubSetOfAnySetIn [] [] = True
isSubSetOfAnySetIn superls ls = or (map (isSubSet ls) superls) 

isSubSet ::(Ord a) => [a] -> [a] -> Bool
isSubSet [] _ = True
isSubSet _ []  = False --is something a subset of nothing?
isSubSet (x:xs) s = if x `elem` s  then isSubSet xs s
                                   else False
                                   
mkRuleFromAttsAndRow :: [Header] -> [String] ->Row -> String  -> Rule
mkRuleFromAttsAndRow heads atts row decTitle= let row' = snd row in
                                        let maybeVals = map ((flip (getMVal heads)) row) atts in
                                          let zippedWithMaybes = zip atts maybeVals in
                                            let ruleReady = removeNothings_pair zippedWithMaybes in
                                              case getMVal heads decTitle row of
                                                Nothing -> Rule ruleReady (decTitle, BoolVal False)  --I put something so this typechecks... Not sure how to actually handle this.
                                                Just dv -> Rule ruleReady (decTitle, dv)
                                              
removeNothings_pair :: [(a, Maybe b)] -> [(a,b)]                                              
removeNothings_pair [] = []
removeNothings_pair (x:xs) = case snd x of
                                Nothing -> removeNothings_pair xs
                                Just v  -> (fst x, v): removeNothings_pair xs 



ruleCovers :: Table -> Rule -> [Int]
ruleCovers table (Rule pairls decPair) = attribValPairsCover table pairls

attribValPairsCover :: Table -> [(String, Value)] -> [Int]
attribValPairsCover table pairls =  let extractors = map fst pairls in
                                       let shoulbes = map (Just . snd) pairls in
                                          join (map (\row -> if (getValsG (tableHeaders table) extractors row ) == shoulbes 
                                                               then [(id row)]
                                                               else []) (tableData table))                                                                                       
ruleIsConsistent :: Table -> Rule -> Bool
ruleIsConsistent table r@(Rule pairls pair) = let covers = ruleCovers table r in
                                                       let betterBeInThisPart = head $ decisionPart table (fst pair) (snd pair) in
                                                          and $ map ((flip elem) betterBeInThisPart) covers  


mytranspose :: [[Maybe Value]] ->  [Row]
mytranspose columnLS = let rowvalsLS = transpose columnLS in 
                        zip [1..] rowvalsLS 
                        
{-
equalFrequency :: (Ord a) => Int -> [a] -> [[a]]
equalFrequency _ [] = []
equalFrequency 1 x = x
equalFrequency i ls = let ls' = sort ls 
   -}                       
occurances :: (Eq a, Num b) => a -> [a] -> b 
occurances _ [] = 0
occurances t (x:xs) = if t == x 
                          then 1 + (occurances t xs)
                          else occurances t xs 
replaceRows :: Table ->[Row] -> Table
replaceRows table newRows = Table (tableHeaders table) newRows

l :: Table -> String -> (Int,Int)
l table dec = let allatts = extractFromHeaders (tableHeaders table) Attribute in
  let decBlocks = compPartG table [dec] 
      x = () --unsafePerformIO $ putStrLn $ "Here is decBlocks: " ++ (show decBlocks)
      in
   case x of 
    () ->    
     (sum (map (length . lowerApprox table dec allatts) decBlocks), length (tableData table)) 

l2 :: Table2 -> Header -> (Int,Int)
l2 t2 = l2' (tableColumns2 t2)

l2' :: M.Map Header (M.Map Int Value) -> Header -> (Int,Int)
l2' columns dec = let decBlocks = compPartG2' columns (S.singleton dec)  
                      headers = M.keysSet columns 
                      attHeaders = S.filter (\(_,y) -> y == Attribute) headers 
                      numEntries = (M.size . snd) (M.elemAt 0 columns)
                      x = S.foldr (\s acc -> ((S.size . (lowerApprox2' columns dec attHeaders)) s) + acc ) 0 decBlocks
                    in 
                (x, numEntries)
isConsistent :: Table -> String -> Bool
isConsistent t dec = let (x,y) = l t dec in 
                        x == y
isConsistent' :: Table -> Bool 
isConsistent' t = let decs = extractFromHeaders (tableHeaders t) Decision in 
                    and (map (isConsistent t) decs)
  
h' :: (Floating a) => Table -> String -> String -> a
h' table dec att = let blocks = compPartG table [att] in 
  let decValBlocks = map (\block -> (map (\i -> getMVal (tableHeaders table) dec (getRow table i)) block)) blocks in
  h decValBlocks
  
h :: (Eq a, Floating b) => [[a]] -> b 
h lsls = let size = sum $ map length lsls in 
  sum ( map (\block -> (   ((fromIntegral (length block)) / (fromIntegral size)) * 
                            (e block)) ) lsls  )
e :: (Floating b, Eq a ) => [a] -> b   
e ls = let uniques = nub ls in 
  foldr (\x acc -> let f = ((fromIntegral (length (filter (==x) ls)))/ (fromIntegral (length ls))) in 
                    (-f)*(logBase 2 f) + acc) 0 uniques  

m ::(Eq a, Floating b) => Int -> [[a]] -> b 
m u lsls = (sum $ map (\block -> ((fromIntegral (length block))/(fromIntegral u))*
                            (e block)) lsls )/ (fromIntegral (length lsls))
                            

                                                        
--  -} 
--table, decisionColumn, attributes, set, answer
lowerApprox :: Table -> String -> [String] -> [Int] -> [Int]
lowerApprox table dec atts set = let a = compPartG table atts in
                                    foldr (\s acc -> if s `isSubSet` set 
                                                        then s ++ acc
                                                        else acc) [] a 
                                                        
lowerApprox2 :: Table2 -> Header -> S.Set Header -> S.Set Int -> S.Set Int 
lowerApprox2 t2 = lowerApprox2' (tableColumns2 t2)

type Columns = M.Map Header (M.Map Int Value)                                                        
lowerApprox2' :: Columns -> Header -> S.Set Header -> S.Set Int -> S.Set Int
lowerApprox2' cols dec atts set = let a = compPartG2' cols atts in 
                                     S.foldr (\s acc -> if s `S.isSubsetOf` set 
                                                        then S.union s acc 
                                                        else acc) S.empty a 
                                                        
upperApprox :: Table -> String -> [String] -> [Int] -> [Int]
upperApprox table dec atts set = let a = compPartG table atts in
    sort . nub $ foldr (\s acc -> (join (filter (s `elem`) a)) ++ acc) [] set
    
    {-
performMerge :: Table -> Table
performMerge table = let allPossibleMerges = 
-}

performPossibleMerges' :: Table -> String -> IO Table
performPossibleMerges' table dec = do 
    let colNames = map fst (tableHeaders table)
        colVals = map ( getColumnValsNoMabies' table) colNames
        pairs = zip colNames colVals 
        mergables = foldr (\pair acc -> case head (snd pair) of 
                                           (Interval _ _) -> pair : acc 
                                           _              -> acc ) [] pairs
        mergablesSorted = map (\(name,ls) -> (name, sort ls)) mergables
        merges = join $ map (\(n,vals) -> zip (repeat n) (map (\(v1,v2) -> 
          case (v1,v2) of 
            (i1@(Interval l1 h1),i2@(Interval l2 h2)) -> 
              ((i1,i2),Interval l1 h2)) (zip vals (tail vals)))) mergablesSorted
         
  --    xxx = unsafePerformIO $ putStrLn $ "Pairs:\n" ++ (show pairs) ++ "\nMergables:\n" ++ (show mergables) ++ "\nMerges:\n" ++ (show merges)
    putStrLn $ "Merge attempts left: " ++ (show (length merges))
    mergedTable <- mergeHelper' table dec merges    
 -- case xxx of 
  --  () -> 
    
   -- liftIO $ putStrLn $ "table before dropSillyColumns: " ++ (show mergedTable)
    --liftIO $ putStrLn $ "That was the before merged tables!!"
    return $ dropSillyColumns mergedTable 
  --merges :: [(String, ((Value,Value),Value))]
performPossibleMerges :: Table ->String -> Table
performPossibleMerges table dec = let colNames = map fst (tableHeaders table)
                                      colVals = map (getColumnValsNoMabies' table) colNames
                                      pairs = zip colNames colVals 
                                      mergables = foldr (\pair acc -> case head (snd pair) of 
                                                                           (Interval _ _) -> pair : acc 
                                                                           _              -> acc ) [] pairs
                                      mergablesSorted = map (\(name,ls) -> (name, sort ls)) mergables
                                      merges = join $ map (\(n,vals) -> zip (repeat n) (map (\(v1,v2) -> 
                                        case (v1,v2) of 
                                          (i1@(Interval l1 h1),i2@(Interval l2 h2)) -> 
                                            ((i1,i2),Interval l1 h2)) (zip vals (tail vals)))) mergablesSorted
                                      mergedTable = mergeHelper table dec merges 
                                  --    xxx = unsafePerformIO $ putStrLn $ "Pairs:\n" ++ (show pairs) ++ "\nMergables:\n" ++ (show mergables) ++ "\nMerges:\n" ++ (show merges)
                                      in 
                                 -- case xxx of 
                                  --  () -> 
                                  dropSillyColumns mergedTable 
                                  --merges :: [(String, ((Value,Value),Value))]
                                  
dropSillyColumns :: Table -> Table 
dropSillyColumns table = let headers = tableHeaders table 
                             atts = extractFromHeaders headers Attribute
                             colLS = map (extractColumn' table) atts 
                             pairs = zip atts colLS 
                             attsToRemove = foldr (\(a,ls) acc -> if length (nub ls) <= 1 then a:acc else acc) [] pairs in 
                         if length attsToRemove > 0 
                          then 
                             let headers' = filter (\(str,ad) -> not ( str `elem` attsToRemove)) headers 
                                 colLS' = foldr (\(att,colvals) acc -> if (att `elem` attsToRemove) then acc else colvals:acc ) [] pairs
                                 newRows = mytranspose (colLS' ++ (map (extractColumn' table) (extractFromHeaders headers Decision))) in 
                             Table headers' newRows
                                 
                          else table 
dropSillyColumns2 :: Table2 -> Table2 
dropSillyColumns2 t2 = let columnMaps = tableColumns2 t2 
                           colHeaders = M.keysSet columnMaps 
                           atts = S.filter (\(_,y) -> y == Attribute) colHeaders
                           newNubs = M.foldrWithKey (\header oldCol acc -> let nubs = nubify oldCol
                                                                               f _ = Just nubs in
                                                                           M.alter f header acc) (tableNubColumns t2) columnMaps 
                           newcols = M.filterWithKey (\header a -> (S.size (newNubs M.! header)) > 1) columnMaps
                           in 
                       let Table2 {..} = t2 in Table2 {tableColumns2 = newcols, tableNubColumns = newNubs, ..}
performPossibleMerges2 :: Table2 -> Header ->IO Table2 
performPossibleMerges2 t2 dec = do let columnMaps = tableColumns2 t2 
                                       colHeaders = M.keysSet columnMaps 
                                       atts = S.filter (\(_,y) -> y == Attribute) colHeaders
                                       newNubs = M.foldrWithKey (\header oldCol acc -> let nubs = nubify oldCol
                                                                                           f _ = Just nubs in
                                                                                           M.alter f header acc) (tableNubColumns t2) columnMaps 
                                       newcols = M.filterWithKey (\header a -> (S.size (newNubs M.! header)) > 1) columnMaps
                                   
                                   let Table2 {..} = t2
                                       nubbedOutTable = Table2 {tableColumns2 = newcols, tableNubColumns = newNubs, ..}
                                       mapOfColumnSetMerges= M.map (\colNubsSet -> 
                                                                   let (min,s2) = S.deleteFindMin colNubsSet
                                                                       (max, set) = S.foldl (\(vprev,acc) v -> (v,(vprev,v):acc)) (min,[] ) s2  
                                                                         in 
                                                                     set
                                                                     ) newNubs 
                                       
                                   performPossibleMerges2Helper nubbedOutTable dec mapOfColumnSetMerges
performPossibleMerges2Helper :: Table2 ->Header -> M.Map Header  [(Value,Value)] ->IO Table2
performPossibleMerges2Helper t2 dec headerMergesMap = do 
                                                      let g = M.keysSet headerMergesMap -- tableHeaders t2 
                                                          relevantOrderedHMap =  M.filter (\a -> S.member a g && ((snd a) == Attribute)) (tableHeaders2 t2)
                                                          cols = M.foldl (\acc h -> 
                                                                            let unit = unsafePerformIO $ putStrLn $ "merging: " ++ (fst h) ++ "\n" in 
                                                                            case unit of 
                                                                              () ->
                                                                                M.insert h (mergeColumn2 t2 dec h (headerMergesMap M.! h)) acc ) M.empty relevantOrderedHMap
                                                       
                                                      let Table2 {..} = t2 in return $ Table2 {tableColumns2 = cols, ..}
                                                      
mergeColumn2 :: Table2 -> Header -> Header -> [(Value,Value)] ->(M.Map Int Value)
mergeColumn2 t2 dec h [] = ((tableColumns2 t2) M.! h)
mergeColumn2 t2 dec h merges =    let (minpair:s) = merges 
                                      columnMap = (tableColumns2 t2) M.! h 
                                      (mergedV,columnMap') = mergeColumn2helper columnMap minpair
                                      f _ = Just columnMap'
                                      origcolums = tableColumns2 t2 
                                      columns' = M.alter f h origcolums
                                      (x,y) = l2' columns' dec 
                                      in 
                                  let Table2 {..} = t2 in
                                  let t2' = Table2 {tableColumns2 = columns',..}
                                      in 
                                  if x == y then mergeColumn2 t2' dec h (case s of 
                                                                          [] -> s
                                                                          _  -> let (q,w) = head s in (mergedV,q):s )
                                            else mergeColumn2 t2 dec h s 
mergeColumn2helper :: M.Map Int Value -> (Value,Value) -> (Value, M.Map Int Value)
mergeColumn2helper colMap (v1,v2) = let ls = case v1 of 
                                              (FloatVal x) -> [x]
                                              (Interval x y) -> [x,y]
                                        ls' = ls ++ (case v2 of 
                                                        (FloatVal x) -> [x]
                                                        (Interval x y) -> [x,y])
                                        mergedV = Interval (minimum ls') (maximum ls')
                                        in 
                                   (mergedV, M.map (\v -> if v == v1 || v == v2 then mergedV else v) colMap )
--performPossibleMerges2 t2 = let                       
nubify :: (Ord a) => M.Map k a -> S.Set a 
nubify = M.foldr (\a acc -> S.insert a acc) S.empty                             
mergeHelper' :: Table ->String -> [(String,((Value,Value),Value))] -> IO Table
mergeHelper' table _ [] = return table 
mergeHelper' table dec ((col1, ((iv1,iv2), niv1@(Interval nl1 nh1))):xs) = do
  putStrLn $ "Merge attempts remaining: " ++ (show (length xs)) 
  let t' = mergeValuesInColumn table col1 [Just iv1,Just iv2] (Just niv1)  
      (x,y) = l t' dec 
  if x == y 
    then 
     if length xs == 0 
      then return t' 
      else do 
           let (col2, (((Interval l1 h1),q@(Interval l2 h2)),(Interval nl2 nh2)))  = head xs  
           if col2 == col1 then do --we are dealing with the same column, otherwise no changes necessary
              let e' = (col2, ( (niv1,q), Interval nl1 nh2))
              mergeHelper' t' dec (e':(tail xs))
           else mergeHelper' t' dec xs 
    else
     mergeHelper' table dec xs                          
mergeHelper :: Table ->String -> [(String,((Value,Value),Value))] -> Table
mergeHelper table _ [] = table 
mergeHelper table dec ((col1, ((iv1,iv2), niv1@(Interval nl1 nh1))):xs) = 
  let t' = mergeValuesInColumn table col1 [Just iv1,Just iv2] (Just niv1)  
      (x,y) = l t' dec in 
  if x == y 
    then 
     if length xs == 0 
      then t' 
      else let (col2, (((Interval l1 h1),q@(Interval l2 h2)),(Interval nl2 nh2)))  = head xs in 
           if col2 == col1 then --we are dealing with the same column, otherwise no changes necessary
              let e' = (col2, ( (niv1,q), Interval nl1 nh2)) in 
              mergeHelper t' dec (e':(tail xs))
           else mergeHelper t' dec xs 
    else
     mergeHelper table dec xs     
mergeValuesInColumn :: Table -> String -> [Maybe Value] -> Maybe Value -> Table
mergeValuesInColumn table colName mvs nv = let colvals = extractColumn' table colName
                                               newcol = map (\mv -> if mv `elem` mvs then nv else mv) colvals
                                                in
                                           replaceColumns table [(colName,newcol)]
                                        
replaceColumns :: Table -> [(String,[Maybe Value])] -> Table 
replaceColumns table colNameNewvalsPairs = let rows = tableData table 
                                               colNames = map fst (tableHeaders table)
                                               newColumns = map (\cn -> case lookup cn colNameNewvalsPairs of 
                                                                          Nothing -> extractColumn' table cn 
                                                                          Just mvs -> mvs) colNames 
                                               newRows = mytranspose newColumns in 
                                           replaceRows table newRows 
table = do 
       tab <- mkTable heads typpes 1 dat
       case tab of
        (Right x)   ->return x
        (Left err)  ->return $ Table [] []
        
partColor = do
            tab <- table
            return $ mySort $ compPartG tab ["Color"]
partTemp = do
            tab <- table
            return $ mySort $ compPartG tab ["Temperature"]     
partAttitute = do
            tab <- table
            return $ mySort $ compPartG tab ["Attitude"]    
            
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs
                           , ys <- combinations (n-1) xs']            
          