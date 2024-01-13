package main

import (
	"fmt"
	filedriver "github.com/goftp/file-driver"
	"github.com/goftp/server"
	"github.com/julienschmidt/httprouter"
	"log"
	"net/http"
)

func Index(w http.ResponseWriter, r *http.Request, _ httprouter.Params) {
	fmt.Fprint(w, "Welcome!\n")
}

func main() {
	mux := httprouter.New()
	mux.GET("/", Index)
	factory := &filedriver.FileDriverFactory{
		RootPath: "server",
		Perm:     server.NewSimplePerm("user", "group"),
	}

	opts := &server.ServerOpts{
		Factory:  factory,
		Port:     2121,
		Hostname: "localhost",
		Auth:     &server.SimpleAuth{Name: "admin", Password: "123456"},
	}

	log.Printf("Starting ftp server on %v:%v", opts.Hostname, opts.Port)
	serv := server.NewServer(opts)
	err := serv.ListenAndServe()
	if err != nil {
		log.Fatal("Error starting server:", err)
	}
}
