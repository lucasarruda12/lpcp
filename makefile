GHC 		:= ghc -dynamic
ALEX 		:= alex
OUT_DIR := dist

lexer: Tokens.x
	alex Tokens.x -o Lexer.hs
	$(GHC) -c Lexer.hs


