package main

import (
	"github.com/gorilla/websocket"
	"github.com/julienschmidt/httprouter"
	"lab9/async/pkg/logger"
	"lab9/sync/api"
	"net/http"
)

func main() {
	mux := httprouter.New()
	l := logger.InitLog()
	conn, _, err := websocket.DefaultDialer.Dial("ws://localhost:8020", nil)
	if err != nil {
		panic("cant connect to the server")
	}
	api.Register(mux, l, conn)
	l.Info("starting server")
	l.Error(http.ListenAndServe(":8080", mux))
}
