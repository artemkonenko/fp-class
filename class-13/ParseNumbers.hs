module ParseNumbers where

import Parser
import SimpleParsers
import Control.Applicative hiding (many, optional)
import Control.Monad


addition' = do
  n <- digit
  char '+'
  m <- digit
  return $ n + m

addition = digit >>= rest
  where
    rest m = (liftM (+m) $ char '+' >> digit) <|> return m


natural = foldl1 (\m n -> m *10 + n) `liftM` many1 digit


integer :: Parser Int
integer = (*) <$> minus <*> natural
  where
    minus = (char '-' >> return (-1)) <|> return 1


intList = bracket "[" "]" $ sepBy (token integer) (symbol ",")
