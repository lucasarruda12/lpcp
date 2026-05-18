GHC 		:= ghc -dynamic
ALEX 		:= alex
OUT_DIR := dist

lexer: tokens.x
	alex tokens.x -o dist/tokens.hs
	$(GHC) -c dist/tokens.hs -outputdir dist


