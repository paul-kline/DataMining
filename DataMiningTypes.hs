{-#LANGUAGE TypeSynonymInstances, FlexibleInstances, ViewPatterns #-}
module DataMiningTypes where

import Prelude hiding (id)
import Text.Read hiding (get)
import Data.List
import Control.Monad
import Control.Monad.State
import System.IO
import qualified Data.Text as Text
--import Monad.StateT

type MyStateTMonad a = StateT DataMiningState IO a 

data DataMiningState = DataMiningState {
                            tableState :: Table
                          } deriving (Show, Eq)

runEvaluate ::  String ->IO (Either String (String, DataMiningState))
runEvaluate fileLoc= do
   handle <- openFile fileLoc ReadMode
   contents <- hGetContents handle
   let linels = lines contents
   if (length linels) < 3 
    then  do 
      let str = "ERROR: input file is too short!"
      putStrLn str
      return (Left str) 
    else do
     let (l1:l2:l3:rest) = linels
     case (readMaybe l1 :: Maybe Int) of
       Nothing        -> do 
         let str = "ERROR: Unable to read first line: " ++ l1 ++ " as Int (number of decisions)" 
         putStrLn str 
         return (Left str)
       (Just numDecs) -> do
          let typesinStrLS = map Text.unpack $ Text.split (==',') (Text.pack l2)          
          let maybes = map readMaybe typesinStrLS
          let maybeLS = sequence maybes
          case maybeLS of
            Nothing      -> do 
              let str = "ERROR: Unable to read second line as TypeIndicators"
              putStrLn str 
              return (Left str)
            (Just types) -> do 
              --rest :: [Text]
              let rest' = map (map Text.unpack) (map (Text.split (==',')) (map Text.pack rest))
              eitherTable <- mkTable (map Text.unpack (Text.split (==',') (Text.pack l3))) types numDecs rest'              
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
      return "nothing"
  return "hello"

lem1 :: MyStateTMonad [Rule]
lem1 = do
        s <- get
        let table = tableState s
        let headerPairs = tableHeaders table
        let decisions = extractFromHeaders headerPairs Decision
        -- :: [[MaybeValues]]
        x <- sequence (map getUniqueValuesFor decisions) --unique vals for each decision column
        --decision, domain pairs
        let decDomPairs = zip decisions x 
        sequence $ map computeLEM1RulesForDecisionColumm decDomPairs
        

computeLEM1RulesForDecisionColumm :: (String, [Maybe Value] -> MyStateTMonad [Rule]
computeLEM1RulesForDecisionColumm (_, []) = []        
computeLEM1RulesForDecisionColumm (dec, ls) = sequence $ map (computeLEM1RulesForDecValue dec) ls 

computeLEM1RulesForDecValue :: String -> Maybe Value -> MyStateTMonad [Rule]
computeLEM1RulesForDecValue decCol maybeValue = do
                                    s <- get
                                    let table = tableState s 
                                    case maybeValue of 
                                        Nothing    -> putStrLn $ "ERROR: attempt to compute rule 'Nothing' value in decision domain: " ++ decCol
                                        (Just val) -> do 
                                          let decPar = decisionPart table decCol  val 
                                          let goodPart = head decPar
                                          let allAtts = extractFromHeaders headerPairs Attribute
                                          computeLem1RulesThatCover goodPart allAtts
                                          
computeLem1RulesThatCover :: [Int] -> [String] > MyStateTMonad [Rule]                                          
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
                   | BoolT deriving (Show, Eq, Ord, Read)
                   
type Row = (Int, [Maybe Value])
id :: Row -> Int
id = fst

strRow :: Row -> String
strRow (i,row) = (show i) ++ "\t" ++ (join (intersperse "\t" (map (\mval -> case mval of
                                                        Nothing -> "NUL"
                                                        (Just val) -> show val) row)))

data Value = StrVal String
           | IntVal Int 
           | BoolVal Bool deriving (Eq, Ord)
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
                  (Just b) -> (Just (BoolVal b)) ) ) 
   in (i,(mVal:(snd (mkRow ts (i,xs)))))

                                    
                                                   

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
ruleSize_Big_Color_Yellow_Feel_Hard = Rule [("Size", (StrVal "big")),("Color",(StrVal "yellow")),("Feel",(StrVal "hard"))] ("Attitude",(StrVal "negative"))

instance Show Rule where
 show (Rule pairLS pair) = "[ ==" ++ (join (intersperse ", ==" (map (show . snd) pairLS))) ++ "] ==> " ++ ( show (snd pair))
 
ruleCovers :: Table -> Rule -> [Int]
ruleCovers table (Rule pairls decPair) = let extractors = map fst pairls in
                                            let shoulbes = map (Just . snd) pairls in
                                               join (map (\row -> if (getValsG (tableHeaders table) extractors row ) == shoulbes then [(id row)]
                                                                                        else []) (tableData table))
ruleIsConsistent :: Table -> Rule -> Bool
ruleIsConsistent table r@(Rule pairls pair) = let covers = ruleCovers table r in
                                                       let betterBeInThisPart = head $ decisionPart table (fst pair) (snd pair) in
                                                          and $ map ((flip elem) betterBeInThisPart) covers
                                   