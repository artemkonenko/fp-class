import Parser
import ParseNumbers
import SimpleParsers
import Control.Applicative hiding (many, optional)
import Control.Monad

data Expr = Con Int | Bin Op Expr Expr
  deriving (Show, Eq)
data Op = Plus | Minus
  deriving (Show, Eq)

{-
   Модифицируйте эту грамматику и парсер таким образом, чтобы они корректно
   распознавали заключённые в скобки числовые литералы.
-}

{-
expr  ::=  nat | '(' expr op expr ')'
op    ::=  '+' | '-'
nat   ::=  {digit}+
digit ::=  '0' | '1' | '2' | ... | '9'
-}

expr :: Parser Expr
expr = token (constant <|>  bracket "(" ")"  binary)
  where
    constant = Con `liftM` natural
    binary = do
      e1 <- expr
      p <- op
      e2 <- expr
      return $ Bin p e1 e2
    op = (symbol "+" >> return Plus) <|> (symbol "-" >> return Minus)

test =  parse expr "2" == parse expr "(2)" &&
        parse expr "((2)+3)" == parse expr "(2+3)" 
