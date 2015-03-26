module DataMiningTools where

import DataMiningTypes
import TableMaker
import Data.List
import Prelude hiding (id)
import Control.Monad
import Control.Monad.State


runEvaluate ::  String ->IO (Either String (String, DataMiningState))
runEvaluate fileLoc= do
              eitherTable <- readInTable fileLoc  
              case eitherTable of
                (Left err)    -> do 
                  putStrLn err
                  return (Left err)
                (Right table) -> do
                  let s0 =  DataMiningState table                   
                  result <- runStateT (evaluate) s0
                  return $ Right result

runEvaluate' :: Table ->IO (String , DataMiningState)
runEvaluate' table = do
   let s0 = DataMiningState table
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
findRWithID rows i = rows !! (i-1)
 {-                                         
findID :: [Row] -> Int -> Row 
--findID [] i = Nothing
findID (x:xs) i = if i == (id x) then x
                               else findID xs i
-}

    
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

replaceRows :: Table ->[Row] -> Table
replaceRows table newRows = Table (tableHeaders table) newRows

l :: Table -> String -> (Int,Int)
l table dec = let allatts = extractFromHeaders (tableHeaders table) Attribute in
  let decBlocks = compPartG table [dec] in 
  (sum (map (length . lowerApprox table dec allatts) decBlocks), length (tableData table)) 
  
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
upperApprox :: Table -> String -> [String] -> [Int] -> [Int]
upperApprox table dec atts set = let a = compPartG table atts in
    sort . nub $ foldr (\s acc -> (join (filter (s `elem`) a)) ++ acc) [] set
                                                        
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