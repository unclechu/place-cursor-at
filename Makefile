all:
	stack build --install-ghc
	stack install

clean:
	stack clean
