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
import Text.Printf
import Data.Function.Memoize
--import Monad.StateT

type MyStateTMonad a = StateT DataMiningState IO a 
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

type DiscState = [(String, (Int,[(Double,Double)]) )]
type DomAttState = [(String, AttInfo)]
data AttInfo = AttInfo {
                 getCuts :: [Double],
                 getIntervals :: [(Double,Double)],
                 getPossibleCuts :: [Double],
                 getAttMinimum :: Double,
                 getAttMaximum :: Double,
                 getMaxCuts :: Int
                 } deriving (Show, Eq)
type LEM2Table = [LEM2Row]                  
data LEM2Row = LEM2Row {
                  attValLEM2Row :: (String,Value),
                  blockLEM2Row  :: [Int],
                  intersectionsLEM2Row :: [[Int]]
} deriving (Eq, Ord, Show)

                    
data Table = Table {
               tableHeaders    :: [Header],
               tableData       :: [Row]
} deriving (Eq, Ord, Bounded)
{-
instance Enum Table where 
 succ a = a 
 pred a = a 
 toEnum _ = Table [] []
 fromEnum _ = 0
 enumFromThen a b = [a,b]
 enumFrom a = [a]
 enumFromTo a b = [a,b]
 enumFromThenTo a b c = [a,b,c]
 -}
instance Memoizable Table where memoize = memoizeFinite

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
                                                        
data Value = StrVal String
           | IntVal Int 
           | BoolVal Bool 
           | DoubleVal Double 
           | Interval Double Double deriving (Eq, Ord)
instance Show Value where
   show (IntVal i) = show i
   show (StrVal str) = str 
   show (BoolVal b) = show b
   show (DoubleVal d) = printf "%.2f" d
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