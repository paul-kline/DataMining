module DataMining where

data Table = Table { columnNames :: [String],
                     tableRows   :: [Row]
                   }
                   
type Row = [(String,Value)]


data Value = ValInt Int
           | ValStr String derving (Eq, Ord, Show)
           
           
mkRow :: [String] ->            
           