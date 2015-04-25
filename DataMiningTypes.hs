{-#LANGUAGE TypeSynonymInstances, FlexibleInstances, ViewPatterns, TemplateHaskell #-}
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
import Text.Printf
import Data.Function.Memoize
import Language.Haskell.TH
import qualified Data.Map as M
import qualified Data.Set as S
--import Monad.StateT

type MyStateTMonad a = StateT DataMiningState IO a 

type MyStateTMonad2 a = StateT DataMiningState2 IO a 

data IntervalMethod = EqualWidth
                    | EqualFrequency
      --              | DominantAtt
                  
data DataMiningState = DataMiningState {
                            tableState :: Table,
                            discretizingTable :: Maybe Table,
            --                discretizingState :: DiscState,
                            getDomAttState    :: DomAttState,
                            verbose :: Bool,
                            stops :: Bool
                     } deriving (Show, Eq)

data DataMiningState2 = DataMiningState2 {
                            stateDec    :: Header,
                            tableState2 :: Table2,
                            discretizingTable2 :: Table2,
                            discretizingState2 :: DiscState2,
                            verbose2 :: Bool,
                            stops2 :: Bool
                     } deriving (Show, Eq)                     
type DiscState = [(String, (Int,[(Float,Float)]) )]
type DomAttState = [(String, AttInfo)]

type DiscState2 = M.Map Header AttInfo2
data AttInfo = AttInfo {
                 getCuts :: [Float],
                 getIntervals :: [(Float,Float)],
                 getPossibleCuts :: [Float],
                 getAttMinimum :: Float,
                 getAttMaximum :: Float,
                 getMaxCuts :: Int
                 } deriving (Show, Eq)
data AttInfo2 = AttInfo2 {
                 getCuts2 :: S.Set Float,
                 getIntervals2 :: S.Set (Float,Float),
                 getPossibleCuts2 :: S.Set Float,
                 getAttMinimum2 :: Float,
                 getAttMaximum2 :: Float,
                 getMaxCuts2 :: Int
                 } deriving (Show, Eq)                 
                 
type LEM2Table = [LEM2Row]                  
data LEM2Row = LEM2Row {
                  attValLEM2Row :: (String,Value),
                  blockLEM2Row  :: [Int],
                  intersectionsLEM2Row :: [[Int]]
} deriving (Eq, Ord, Show)

data Table = Table {
               tableHeaders :: [Header],
               tableData    :: [Row]
               } deriving (Eq, Ord)
data Table2 = Table2 {
               tableHeaders2    :: M.Map Int Header, --[Header],
               --tableAtts    :: S.Set Header,
               tableData2       :: M.Map Int (M.Map Header Value),
               tableColumns2     :: M.Map Header (M.Map Int Value),
               tableNubColumns   :: M.Map Header (S.Set Value)
} deriving (Eq, Ord)
emptyTable2 = Table2 M.empty M.empty M.empty M.empty
{-
instance Show Table2 where
    show (Table2 h2 d2 _ _) = 
                    (join (intersperse "\t" (map fst (M.elems h2 )))) ++ "\n" ++ 
                                (join (intersperse "\n" (M.elems ( M.map (strRow2 h2) d2)))) where
                                  strRow2 h2 headervalMap = join (intersperse "\t" (M.elems ( M.map (\h -> (show (headervalMap M.! h))) h2)))
-}
instance Show Table2 where
    show (Table2 h2 _ columns _) = let numrows = M.size (snd (M.elemAt 0 columns)) in 
                    (join (intersperse "\t" (map fst (M.elems h2 )))) ++ "\n" ++ 
                     (join (intersperse "\n"  (map (\i -> M.foldr (\header acc -> case M.lookup header columns of 
                                                                                    Nothing -> acc 
                                                                                    Just c  ->(show (c M.! i)) ++ "\t" ++ acc  ) "" h2 ) [1..numrows])))
                            --    (join (intersperse "\n" ( ( M.foldr (\ v acc -> ((show v) ++ "\t") ++ acc) "" columns)))) where
                              --    strRow2 h2 headervalMap = join (intersperse "\t" (M.elems ( M.map (\h -> (show (headervalMap M.! h))) h2)))
                                  
                                  
tableToTable2 :: Table -> Table2
tableToTable2 table = let headers = tableHeaders table
                          headers2 = M.fromList (zip [1..] headers)
                          data2    = M.fromList (map (\(i,mvals) -> (i, (M.fromList (zip headers (unMaybeLS mvals)))))(tableData table))
                          columns2 = M.fromList (map (\h -> (h,(getCol data2 h))) headers  )
                          nubs = M.map (S.fromList . M.elems) columns2
                          --atts2 = S.fromList $ filter (\(x,y) -> y == Attribute)
                          in 
                      Table2 headers2 data2 columns2 nubs 
t2Tot :: Table2 -> Table
t2Tot t2 = let cols = tableColumns2 t2 
               allheaders = M.elems (tableHeaders2 t2)
               colHeads = M.keysSet cols 
               relevantHeaders = filter (\x -> x `S.member` colHeads) allheaders 
               numrows = M.size (snd (M.elemAt 0 cols))
               rows = (map (\i -> (i, foldr (\header acc -> case M.lookup header cols of 
                                                                                    Nothing -> acc 
                                                                                    Just c  ->(Just (c M.! i)):acc  ) [] relevantHeaders) ) [1..numrows])
               in 
           Table relevantHeaders rows 
unMaybeLS :: [Maybe a] -> [a]
unMaybeLS = foldr (\mv acc -> case mv of 
                                    Nothing -> acc 
                                    Just v  -> v:acc) []
                                            
getCol :: M.Map Int (M.Map Header Value) -> Header ->M.Map Int Value
getCol irowsMap key = M.map (M.! key) irowsMap
                          {-
instance Enum Table where 
 succ a = a 
 pred a = a 
 toEnum _ = Table [] []
 fromEnum _ = 0
 enumFromThen a b = [a,b]
 enumFrom a = [a]
 enumFromTo a b = [a,b]
 enumFromThenTo a b c = [a,b,c]-}

--instance Memoizable Table where memoize = memoizeFinite
-- $(deriveMemoizable ''Table)
-- $(deriveMemoizable ''AttDec)
instance Memoizable AttDec where memoize = memoizeFinite

instance Show Table where
   show (Table headers dattt) = "Table:\n" ++ "ID\t" ++ (join (intersperse "\t" (map fst headers))) ++ "\n" ++ 
                                (join (intersperse "\n" (map strRow dattt))) 

type Header = (String,AttDec) 

data AttDec = Attribute 
            | Decision deriving (Eq, Show, Ord, Enum, Bounded)
 
data TypeIndicator = IntT
                   | StringT
                   | BoolT 
                   | FloatT deriving (Show, Eq, Ord, Read)
                   
type Row = (Int, [Maybe Value]) --deriving (Bounded)
id :: Row -> Int
id = fst

strRow :: Row -> String
strRow (i,row) = (show i) ++ "\t" ++ (join (intersperse "\t" (map (\mval -> case mval of
                                                        Nothing -> "NUL"
                                                        (Just val) -> show val) row)))
strRow' :: Row -> String 
strRow' (i,row) = (join (intersperse "\t" (map (\mval -> case mval of
                                                        Nothing -> "NUL"
                                                        (Just val) -> show val) row)))
                                                        
strRow2 :: M.Map Int Header -> (Int,[(Header,Value)]) -> String 
strRow2 iheaderMap (i, ls) = join (intersperse "\t" (map show ls))
                                                        
data Value = StrVal String
           | IntVal Int 
           | BoolVal Bool 
           | FloatVal Float 
           | Interval Float Float deriving (Eq, Ord)



instance Show Value where
   show (IntVal i) = show i
   show (StrVal str) = str 
   show (BoolVal b) = show b
   show (FloatVal d) = printf "%.2f" d
   show (Interval dlow dhigh) = (printf "%.3f" dlow) ++ ".." ++ (printf "%.3f" dhigh)   
data Rule = Rule [(String,Value)] (String,Value)

ruleSize_Big_Color_Yellow_Feel_Hard = Rule [("Size", (StrVal "big")),("Color",(StrVal "yellow")),("Feel",(StrVal "hard"))] ("Attitude",(StrVal "negative"))

instance Show Rule where
 show (Rule pairLS pair) = "[ ==" ++ (join (intersperse ", ==" (map (show . snd) pairLS))) ++ "] ==> " ++ ( show (snd pair))
  

  
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
    
    
    
instance Num Value where
    (FloatVal d1) + (FloatVal d2) = FloatVal (d1 + d2)
    (Interval l1 h1) + (Interval l2 h2) = Interval (minimum [l1,l2]) (maximum [h1, h2])
    (FloatVal d1) - (FloatVal d2) = FloatVal (d1 - d2)
    (FloatVal d1) * (FloatVal d2) = FloatVal (d1 * d2)
    negate (FloatVal d)= FloatVal (negate d)
    abs (FloatVal d) = FloatVal (abs d)
    signum (FloatVal d) = FloatVal (signum d)
    fromInteger i  =  (FloatVal (fromInteger i))   