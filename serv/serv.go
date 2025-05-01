package main

import (
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
)

func uploadHandler(w http.ResponseWriter, r *http.Request) {
	err := r.ParseMultipartForm(10 << 20) // 10 MB max
	if err != nil {
		http.Error(w, "Erreur lors du parsing du fichier", http.StatusBadRequest)
		return
	}
	file, handler, err := r.FormFile("file")
	if err != nil {
		http.Error(w, "Erreur lors de la récupération du fichier", http.StatusBadRequest)
		return
	}
	defer file.Close()
	relpath := r.FormValue("relpath")
	dstPath := filepath.Join("store", filepath.Clean(relpath))

	err = os.MkdirAll(filepath.Dir(dstPath), os.ModePerm)

	if err != nil {
		http.Error(w, "Erreur création dossier", http.StatusInternalServerError)
		return
	}

	dst, err := os.Create(dstPath)
	if err != nil {
		http.Error(w, "Erreur création fichier", http.StatusInternalServerError)
		return
	}
	defer dst.Close()
	
	_, err = io.Copy(dst, file)
	if err != nil {
		http.Error(w, "Erreur écriture fichier", http.StatusInternalServerError)
		return
	}

	fmt.Printf("Fichier %s sauvegardé avec succès.\n", handler.Filename)
}


func main() {
	fs := http.FileServer(http.Dir("."))
	http.Handle("/", fs)
	http.HandleFunc("/upload", uploadHandler)
	fmt.Printf("Serveur démarré à http://localhost:8080\n")
	_ = http.ListenAndServe("localhost:8080", nil)	
}
