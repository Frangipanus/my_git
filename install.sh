#!/bin/sh

set -e

echo "Compilation du ocaml ..."
dune build

echo "Création de /opt/bite si besoin"
sudo mkdir -p /opt/bite

echo "Compilation du client vers /opt/bite/client..."
go build -o client src/client.go

sudo mv client /opt/bite/

echo "Bite a été installé avec succès"

