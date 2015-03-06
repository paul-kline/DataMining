{-#LANGUAGE TypeSynonymInstances, FlexibleInstances, ViewPatterns #-}
module DataMiningTypes where

import Prelude hiding (id)
import Text.Read hiding (get)
import Data.List
import Control.Monad
import Control.Monad.State
import System.IO
import qualified Data.Text as Text
import Data.Function  hiding (id)
import qualified Data.Text.Lazy as L
--import Monad.StateT

type MyStateTMonad a = StateT DataMiningState IO a 

data DataMiningState = DataMiningState {
                            tableState :: Table
                     } deriving (Show, Eq)

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

type LEM2Table = [LEM2Row]                  
data LEM2Row = LEM2Row {
                  attValLEM2Row :: (String,Value),
                  blockLEM2Row  :: [Int],
                  intersectionsLEM2Row :: [[Int]]
} deriving (Eq, Ord, Show)

{-
type Goal = [Int]
type MasterGoal = [Int]
computeLEM2WithTableGoal ::  LEM2Table -> MasterGoal -> Goal -> MyStateTMonad [Rule]
computeLEM2WithTableGoal lem2table masterGoal goal = do
   let intersections = map (\row -> intersect (blockLEM2Row row) goal) lem2table
   let len = length $ maximumBy (compare `on` length) intersections
   let biggies = filter (\a -> (length a) == len) intersections
   let chosenOne = if length biggies == 1 
      then head biggies
      else 
   return []
-}
        
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
    
typpes = [StringT, StringT, StringT, StringT, StringT]
heads = ["Size", "Color", "Feel", "Temperature", "Attitude"]

dat = [[ "big", "yellow", "soft", "low", "positive"],
       [ "big", "yellow", "hard", "high", "negative"],
       [ "medium", "yellow", "soft", "high", "positive"],
       [ "medium", "blue", "hard", "high", "positive"],
       [ "medium", "blue", "hard", "high", "positive"],
       [ "medium", "blue", "soft", "low", "negative"],
       [ "big", "blue", "hard", "low", "so-so"],
       [ "big", "blue", "hard", "high", "so-so"] ]



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
data Table = Table {
               tableHeaders    :: [Header],
               tableData       :: [Row]
} deriving (Eq, Ord)

instance Show Table where
   show (Table headers dattt) = "Table:\n" ++ "ID\t" ++ (join (intersperse "\t" (map fst headers))) ++ "\n" ++ 
                                (join (intersperse "\n" (map strRow dattt))) 

type Header = (String,AttDec)


data AttDec = Attribute 
            | Decision deriving (Eq, Show, Ord)
 
data TypeIndicator = IntT
                   | StringT
                   | BoolT 
                   | DoubleT deriving (Show, Eq, Ord, Read)
                   
type Row = (Int, [Maybe Value])
id :: Row -> Int
id = fst

strRow :: Row -> String
strRow (i,row) = (show i) ++ "\t" ++ (join (intersperse "\t" (map (\mval -> case mval of
                                                        Nothing -> "NUL"
                                                        (Just val) -> show val) row)))

data Value = StrVal String
           | IntVal Int 
           | BoolVal Bool 
           | DoubleVal Double deriving (Eq, Ord)
instance Show Value where
   show (IntVal i) = show i
   show (StrVal str) = str 
   show (BoolVal b) = show b

   
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
           
   
 {-  
getPartition :: Table -> [String] -> [[Int]]
getPartition table []     = []
getPartition table (x:xs) =
   -}
                    --extractor
                    
                    
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

data Rule = Rule [(String,Value)] (String,Value)
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
                                
ruleSize_Big_Color_Yellow_Feel_Hard = Rule [("Size", (StrVal "big")),("Color",(StrVal "yellow")),("Feel",(StrVal "hard"))] ("Attitude",(StrVal "negative"))

instance Show Rule where
 show (Rule pairLS pair) = "[ ==" ++ (join (intersperse ", ==" (map (show . snd) pairLS))) ++ "] ==> " ++ ( show (snd pair))
 
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
                                   