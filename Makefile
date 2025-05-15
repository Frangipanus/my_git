.PHONY: all clean build client

all: clean build client mvbin

clean:
	dune clean

build:
	dune build

client:
	go build -o bin/client src/client.go

mvbin:
	mv src/bite.exe bin/bite
