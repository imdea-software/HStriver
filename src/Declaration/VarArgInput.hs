{-# Language GADTs #-}
module Declaration.VarArgInput where
import Declaration.Declaration
import Data.Aeson

-- Shamelessly "inspired" by https://wiki.haskell.org/Varargs

class PrintAllType t where
    printAll' :: [String] -> t

instance (FromJSON a, Streamable a) => PrintAllType (Declaration a) where
    printAll' acc = NewInput (head acc) (tail acc)

instance (Show a, PrintAllType r) => PrintAllType (a -> r) where
    printAll' acc = \x -> printAll' (acc ++ [show x])

input :: (PrintAllType t) => String -> t
input n = printAll' [n]
