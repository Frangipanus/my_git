.PHONY: install

install:
	dune build
	go build -o bin/client src/client.go
