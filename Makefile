.PHONY: all clean build client

all: clean build client

clean:
	dune clean

build:
	dune build

client:
	go build -o bin/client src/client.go

