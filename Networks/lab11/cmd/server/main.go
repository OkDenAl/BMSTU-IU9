package main

import (
	"github.com/julienschmidt/httprouter"
	"log"
	"net/http"
)

func Register(mux *httprouter.Router) {
	mux.GET("/dashboard", handleDashboard)
}

func handleDashboard(w http.ResponseWriter, r *http.Request, p httprouter.Params) {
	http.ServeFile(w, r, "web/dashboard.html")
}

func main() {
	mux := httprouter.New()
	Register(mux)
	log.Fatal(http.ListenAndServe(":8070", mux))
}
