package server

import (
	"github.com/julienschmidt/httprouter"
	"net/http"
)

func Register(mux *httprouter.Router) {
	mux.GET("/dashboard", handleDashboard)
}

func handleDashboard(w http.ResponseWriter, r *http.Request, p httprouter.Params) {
	http.ServeFile(w, r, "web/dashboard.html")
}
