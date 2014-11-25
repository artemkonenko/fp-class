module SimpleParsers where

import Parser
import Control.Applicative hiding (many, optional)
import Control.Monad
import Data.Char

getc :: Parser Char
getc = Parser f
  where
    f [] = []
    f (c:cs) = [(c, cs)]

sat :: (Char -> Bool) -> Parser Char
sat pr = do
  c <- getc
  guard $ pr c
  return c

char :: Char -> Parser ()
char x = sat (==x) >> return ()

string :: String -> Parser ()
string = mapM_ char

lower :: Parser Char
lower = sat isLower

digit :: Parser Int
digit = digitToInt `liftM` sat isDigit

lowers :: Parser String
lowers = (:) <$> lower <*> lowers <|> return ""

optional :: a -> Parser a -> Parser a
optional v p = p <|> return v

many :: Parser a -> Parser [a]
many p = many1 p <|> return []

many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

space :: Parser ()
space = many (sat isSpace) >> return ()

sepBy1 p sep = (:) <$> p <*> many (sep >> p)

sepBy p sep = optional [] (sepBy1 p sep)

symbol s = space >> string s

token p = space >> p

bracket op cl p = do
  symbol op
  x <- p
  symbol cl
  return x

