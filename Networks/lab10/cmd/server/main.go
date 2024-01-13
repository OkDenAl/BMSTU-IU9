package main

import (
	"github.com/julienschmidt/httprouter"
	"lab10_my/api/server"
	"log"
	"net/http"
)

func main() {
	mux := httprouter.New()
	server.Register(mux)
	log.Fatal(http.ListenAndServe(":8080", mux))
}
