package main

import (
	"bytes"
	"fmt"
	"io"
	"mime/multipart"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"encoding/json"
)


func findBite(startPath string) (string, error) {
	current := startPath
	for {
		entries, err := os.ReadDir(current)
		if err != nil {
			return "", err
		}
		for _, entry := range entries {
			if entry.IsDir() && entry.Name() == ".bite" {
				return (current+"/.bite/"), nil
			}
		}
		// Remonter d'un niveau
		parent := filepath.Dir(current)
		if parent == current {
			break
		}
		current = parent
	}
	return "", nil 
}

func getUrl(folderPath string) string {
	b, _ := os.ReadFile(folderPath+"config")
	return strings.Split(strings.Split(string(b), "'")[1], "'")[0]
}

func uploadFile(filePath string, relPath string, url string) error {
	fmt.Printf("Upload de %s", filePath)

	file, err := os.Open(filePath)
	if err != nil {
		return fmt.Errorf("erreur ouverture fichier : %w", err)
	}
	defer file.Close()
	var body bytes.Buffer
	writer := multipart.NewWriter(&body)
	part, err := writer.CreateFormFile("file", relPath)
	if err != nil {
		return fmt.Errorf("erreur création de fichier : %w", err)
	}
	_, err = io.Copy(part, file)
	if err != nil {
		return fmt.Errorf("erreur copie fichier : %w", err)
	}
	_ = writer.WriteField("relpath", relPath)
	err = writer.Close()
	if err != nil {
		return fmt.Errorf("erreur fermeture writer : %w", err)
	}
	req, err := http.NewRequest("POST", url, &body)
	if err != nil {
		return fmt.Errorf("erreur création requête : %w", err)
	}
	req.Header.Set("Content-Type", writer.FormDataContentType())
	client := &http.Client{}
	resp, err := client.Do(req)
	if err != nil {
		return fmt.Errorf("erreur envoi requête : %w", err)
	}
	defer resp.Body.Close()
	fmt.Printf(" [*]\n")
	return nil
}


func iterupload (folderPath string)  {
	return
}

func downloadFile(url, dest string) error {
	resp, err := http.Get(url)
	if err != nil {
		return err
	}
	defer resp.Body.Close()
	err = os.MkdirAll(filepath.Dir(dest), os.ModePerm)
	if err != nil {
		return err
	}
	out, err := os.Create(dest)
	if err != nil {
		return err
	}
	defer out.Close()
	_, err = io.Copy(out, resp.Body)
	return err
}

func main() {
	// Faudra bouger ça en gros, pour mettre tous les fichiers suivis par le commit
	args := os.Args[1:]
	if args[0] == "push" {
		dirn, errg := os.Getwd()
		if errg != nil {
			fmt.Println(errg) 
		}
		folderPath, errf := findBite(dirn)
		if errf != nil {
			fmt.Println(errf) 
		}
		// changer ça quand on déploie, par exemple eleves.ens.fr/
		remoteUrl := fmt.Sprintf("http://%s:8080/upload", getUrl(folderPath))
		// Parcourt tous les fichiers du dossier
		err := filepath.Walk(folderPath,
			(func (path string, info os.FileInfo, err error) error {
				if err != nil {
					return err
				}
				if info.IsDir() {
					return nil
				}
				relPath, err := filepath.Rel(folderPath, path)
				if err != nil {
					return err
				}
				return uploadFile(path,relPath,remoteUrl)
			}))	
		if err != nil {
			fmt.Printf("Erreur lors de l'upload : %v\n", err)
		}
	}
	if args[0] == "backup" {
		if len(args) < 2 {
			fmt.Printf("Nom du serveur distant manquant. \nbite backup <url>")
		}
		baseUrl := args[1]
		listUrl := fmt.Sprintf("http://%s:8080/list", baseUrl)
		resp, err := http.Get(listUrl)
		if err != nil {
			panic(err)
		}
		defer resp.Body.Close()
		var files []string
		err = json.NewDecoder(resp.Body).Decode(&files)
		if err != nil {
			panic(err)
		}
		iterUrl := fmt.Sprintf("http://%s:8080/store/", baseUrl)
		for _, file := range files {
			err := downloadFile(iterUrl+file, ".bite/"+file)
			if err != nil {
				fmt.Println("Erreur lors du téléchargement", err)
			}
			fmt.Printf("%s téléchargé [*]\n", file)
		}
	}
}
	
