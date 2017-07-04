all: build

build: client-build server-build

client-build:
	stack exec -- runhaskell -isrc elm/src/Generated/GenerateElm.hs

server-build:
	stack build

run: server-build
	stack exec haskellServer-exe
