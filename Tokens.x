{ 
module Tokens where 
}

%wrapper "basic" 
$digit = 0-9     
-- digits 
$alpha = [a-zA-Z]    
-- alphabetic characters
$sym = [ \! \@ \# \$ ]

tokens :-
$white+       ; 
  "--".*        ; 
  \-            { \s -> TokenDif } -- what to exclude
  \=            { \s -> TokenEq } 
  \(          { \s -> TokenLParen }
  \)          { \s -> TokenRParen }
  $alpha [$alpha $digit \_ \’]*   { \s -> TokenVar s } 

{ 
-- Each action has type :: String -> Token 
-- The token type: 
data Token = 
  TokenEx          |
  TokenDif         |
  TokenEq          |
  TokenVar String  | 
  TokenLParen      |
  TokenRParen       
  deriving (Eq,Show) 

}