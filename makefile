myinterpreter: myinterpreter.hs Tokens.hs Grammar.hs
ghc --make myinterpreter
Tokens.hs: Tokens.x
alex Tokens.x
Grammar.hs: Grammar.y
happy Grammar.y