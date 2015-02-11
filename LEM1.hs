{-#LANGUAGE TypeSynonymInstances, FlexibleInstances, ViewPatterns #-}
module LEM1 where

import Prelude hiding (id)
import Data.List
import Control.Monad (join)
columnName = ["Size", "Color", "Feel", "Temperature", "Attitude"]

row1 = Row 1 "big" "yellow" "soft" "low" "positive"
row2 = Row 2 "big" "yellow" "hard" "high" "negative"
row3 = Row 3 "medium" "yellow" "soft" "high" "positive"
row4 = Row 4 "medium" "blue" "hard" "high" "positive"
row5 = Row 5 "medium" "blue" "hard" "high" "positive"
row6 = Row 6 "medium" "blue" "soft" "low" "negative"
row7 = Row 7 "big" "blue" "hard" "low" "so-so"
row8 = Row 8 "big" "blue" "hard" "high" "so-so"

dat = [row1,row2,row3,row4,row5,row6,row7,row8]
partSize = mySort $ compPart dat [size]
partColor = mySort $ compPart dat [color]
partFeel = mySort $ compPart dat [feel]
partTemp = mySort $ compPart dat [temp]

partSize_Color = mySort $ compPart dat [size,color]
partSize_Feel = mySort $ compPart dat [size,feel]
partSize_Temp = mySort $ compPart dat [size,temp]
partColor_Feel = mySort $ compPart dat [color,feel]
partColor_Temp = mySort $ compPart dat [color,temp]
partFeel_Temp = mySort $ compPart dat [feel,temp]

partSize_Color_Feel = mySort $ compPart dat [size,color,feel]
partSize_Color_Temp = mySort $ compPart dat [size,color,temp]
partSize_Feel_Temp = mySort $ compPart dat [size,feel,temp]
partColor_Feel_Temp = mySort $ compPart dat [color,feel,temp]

aStar = mySort $ compPart dat [size,color,feel,temp]
dStar = mySort $ compPart dat [att]

partAttPlus = mySort $ decisionPart dat att "positive"
partAttSoSo = mySort $ decisionPart dat att "so-so"
partAttNegative = mySort $ decisionPart dat att "negative"

data Row = Row { id    :: Int,
             size  :: String,
             color :: String,
             feel  :: String,
             temp  :: String,
             att   :: String
} deriving (Show, Eq, Ord)

type Attribute = (Row -> String)
type Decision  = (Row -> String)
type Extractor = (Row -> String)

  
  
{-
getPart :: [Row] -> Extractor -> String -> [Int]
getPart [] _ _ = []
getPart (x:xs) f str = if (f x) == str then (id x) : (getPart xs f str)
                                       else getPart xs f str 
                                       
-}
                                       
decisionPart :: [Row] -> Extractor -> String -> [[Int]]
decisionPart [] _ _ = []
decisionPart table f str = let part = compPart table [f] in
                               let mID = findAnIDWithAtt table f str in
                                   let interestingPart = findPartWithID mID part in
                                    let nonInterestingPart = joinPartsNotContainingID mID part in
                                        [interestingPart, nonInterestingPart]

findAnIDWithAtt :: [Row] -> Extractor -> String -> Int         
--findAnIDWithAtt []
findAnIDWithAtt (x:xs) f str = if (f x) == str then id x
                                               else findAnIDWithAtt xs f str 

findPartWithID :: Int -> [[Int]] -> [Int]
findPartWithID i (x:xs) = if i `elem` x then x
                                        else findPartWithID i xs
                                        
joinPartsNotContainingID :: Int -> [[Int]] -> [Int]
joinPartsNotContainingID _ [] = []
joinPartsNotContainingID i (x:xs) = if i `elem` x then (joinPartsNotContainingID i xs)
                                                  else x ++ (joinPartsNotContainingID i xs)
                                       
                                
   
   
compPart :: [Row] -> [Extractor] -> [[Int]]
compPart table fs = compPart' table fs []

compPart' :: [Row] -> [Extractor] -> [[Int]] -> [[Int]]
compPart' [] fs curPart = curPart
compPart' (x:xs) fs curPart = compPart' xs fs (addAppropriately x fs curPart) 

addAppropriately :: Row -> [Extractor] -> [[Int]] -> [[Int]]
addAppropriately row fs [] = [[id row]]
addAppropriately row fs ((x:xs):xs') = if ( (getVals fs row) == (getVals fs (findID dat x)) ) then ((id row):x:xs):xs'
                                                                                              else ((x:xs):(addAppropriately row fs xs'))
getVals :: [Extractor] -> Row -> [String]
getVals [] r = []
getVals (f:fs) r = (f r):(getVals fs r)

                                          
findID :: [Row] -> Int -> Row 
--findID [] i = Nothing
findID (x:xs) i = if i == (id x) then x
                               else findID xs i
                               
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

data Rule = Rule [(Extractor,String)] (Extractor,String)
ruleSize_Big_Color_Yellow_Feel_Hard = Rule [(size, "big"),(color,"yellow"),(feel,"hard")] (att, "negative")

instance Show Rule where
 show (Rule pairLS pair) = "[ ==" ++ (join (intersperse ", ==" (map (snd) pairLS))) ++ "] ==> " ++ (snd pair)
 
ruleCovers :: [Row] -> Rule -> [Int]
ruleCovers table (Rule pairls decPair) = let extractors = map fst pairls in
                                            let shoulbes = map snd pairls in
                                               join (map (\row -> if (getVals extractors row ) == shoulbes then [(id row)]
                                                                                        else []) table)
ruleIsConsistent :: [Row] -> Rule -> Bool
ruleIsConsistent table r@(Rule pairls pair) = let covers = ruleCovers table r in
                                                       let betterBeInThisPart = head $ decisionPart table (fst pair) (snd pair) in
                                                          and $ map ((flip elem) betterBeInThisPart) covers
