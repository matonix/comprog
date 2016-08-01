{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative hiding ((<|>))
import           Control.Monad
import           Data.List
import           Data.Tuple
import           Data.Int
import           Data.Char
import           Data.Function
-- import           Data.ByteString (ByteString)
-- import qualified Data.ByteString.Char8 as C
-- import           Data.ByteString.Char8 ()
-- import           Data.Vector.Unboxed ((//), (++), (!), (!?))
-- import qualified Data.Vector.Unboxed as U
-- import           Data.IntMap.Strict (IntMap)
-- import qualified Data.IntMap.Strict as IntMap
-- import           Data.Sequence ((|>), (<|), (><),ViewR((:>)), ViewL((:<)))
-- import qualified Data.Sequence as S
import           Text.Parsec
import           Text.Parsec.String

data Exp = Add Exp Exp | Mul Exp Exp | Nat Int deriving Show

myread = read :: String -> Int
main = do
  xs <- getLine
  print $ solve xs 

solve :: String -> Int
solve = calc . parse expr ""

calc :: Either ParseError Exp -> Int
calc e = case e of
  Left _   -> 0
  Right ex -> eval ex

eval :: Exp -> Int
eval ex = case ex of
  Add a b -> eval a + eval b
  Mul a b -> eval a * eval b
  Nat a   -> if a==0 then 0 else 1

-- expr ::= term ('+' expr | e)
expr :: Parser Exp
expr = do
  t <- term
  Add t <$> (char '+' *> expr) <|> pure t

-- expr ::= nat ('+' term | e)
term  :: Parser Exp
term = do
  t <- nat
  Mul t <$> (char '*' *> term) <|> pure t

-- nat ::= '0' | '1' | ...
nat :: Parser Exp
nat = Nat . charToInt <$> oneOf ['0'..'9'] where
  charToInt c = ord c - ord '0'