GHC 		:= ghc -dynamic
ALEX 		:= alex
OUT_DIR := dist

lexer: Tokens.x
	alex Tokens.x -o dist/Lexer.hs
	$(GHC) -c dist/Lexer.hs -outputdir dist


