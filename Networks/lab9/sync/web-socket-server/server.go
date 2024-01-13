package main

import (
	"github.com/gorilla/websocket"
	"log"
	"net/http"
)

var upgr = websocket.Upgrader{
	WriteBufferSize: 1024,
	ReadBufferSize:  1024,
	CheckOrigin: func(r *http.Request) bool {
		return true
	}}

func reader(conn *websocket.Conn) {
	for {
		msgType, msg, err := conn.ReadMessage()
		if err != nil {
			log.Println(err)
			return
		}
		log.Printf("Полученное сообщение от клиента: %s", string(msg))
		err = conn.WriteMessage(msgType, msg)
		if err != nil {
			log.Println(err)
			return
		}
	}
}

func handler(w http.ResponseWriter, r *http.Request) {
	ws, err := upgr.Upgrade(w, r, nil)
	if err != nil {
		log.Println(err)
		return
	}
	reader(ws)
}

func main() {
	http.HandleFunc("/", handler)
	log.Fatal(http.ListenAndServe(":8020", nil))
}
