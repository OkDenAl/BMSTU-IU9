package main

import (
	"encoding/json"
	"github.com/gorilla/websocket"
	"lab5/entity"
	"log"
	"net/http"
	"strconv"
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
		matrix := entity.Matrix{}
		err = json.Unmarshal(msg, &matrix)
		if err != nil {
			log.Println(err)
			return
		}
		log.Println("Полученная матрица:", matrix)
		res := strconv.Itoa(matrix.Det())
		err = conn.WriteMessage(msgType, []byte(res))
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
