{-#LANGUAGE TypeSynonymInstances, FlexibleInstances, ViewPatterns #-}
module DataMiningTypes where

import Text.Read
import Data.List
import Control.Monad
typpes = [IntT, StringT, StringT, StringT, StringT, StringT]
heads = ["ID", "Size", "Color", "Feel", "Temperature", "Attitude"]

dat = [["1", "big", "yellow", "soft", "low", "positive"],
       ["2", "big", "yellow", "hard", "high", "negative"],
       ["3" ,"medium", "yellow", "soft", "high", "positive"],
       ["4", "medium", "blue", "hard", "high", "positive"],
       ["5", "medium", "blue", "hard", "high", "positive"],
       ["6", "medium", "blue", "soft", "low", "negative"],
       ["7", "big", "blue", "hard", "low", "so-so"],
       ["8", "big", "blue", "hard", "high", "so-so"] ]




data Table = Table {
               tableHeaders    :: [Header],
               tableData       :: [Row]
} deriving (Eq, Ord)

instance Show Table where
   show (Table headers dattt) = "Table:\n" ++ (join (intersperse "\t" (map fst headers))) ++ "\n" ++ 
                                (join (intersperse "\n" (map strRow dattt))) 

type Header = (String,AttDec)


data AttDec = Attribute 
            | Decision deriving (Eq, Show, Ord)
 
data TypeIndicator = IntT
                   | StringT
                   | BoolT
                   
type Row = [Maybe Value]
strRow :: Row -> String
strRow row = join (intersperse "\t" (map (\mval -> case mval of
                                                        Nothing -> "NUL"
                                                        (Just val) -> show val) row))

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
           
   
   
   
getMVal :: [Header] -> Row -> String -> Maybe Value
getMVal headers row str = let mPos = getPosition headers str in
                             case mPos of
                                  Nothing -> Nothing
                                  (Just pos) -> row !! pos 

getPosition :: [Header] -> String -> Maybe Int 
getPosition [] _ = Nothing
getPosition (x:xs) str = if fst x == str then Just 0 
                                         else case getPosition xs str of
                                                   Nothing -> Nothing
                                                   (Just pos) ->(Just (1 + pos ))
mkData :: [[String]] -> [TypeIndicator] -> [Row]
mkData [] _       = []
mkData lsls types = map (mkRow types) lsls

mkRow :: [TypeIndicator] -> [String] -> Row
mkRow _ [] = []
mkRow (t:ts) (x:xs) = 
   let mVal = (case t of 
               IntT    -> (case readMaybe x :: Maybe Int of
                              Nothing    -> Nothing
                              (Just int) -> (Just (IntVal int)))
               StringT -> (if (length x) > 0 then (Just (StrVal x))
                                             else Nothing)
               BoolT   -> (case readMaybe x :: Maybe Bool of
                  Nothing  -> Nothing
                  (Just b) -> (Just (BoolVal b)) ) ) 
   in (mVal:(mkRow ts xs))

                                    
                                                   

rowsSameLength :: [[String]] -> Maybe Int
rowsSameLength []     = Just 0
rowsSameLength (x:[]) = Just (length x)
rowsSameLength (x:xs) = case rowsSameLength xs of
                             Nothing    -> Nothing
                             (Just len) -> if (length x) == len then (Just len)
                                                                else Nothing
  