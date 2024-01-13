package main

import (
	"bytes"
	"github.com/gorilla/websocket"
	"github.com/julienschmidt/httprouter"
	"log"
	"net/http"
	"os/exec"
	"time"
)

func Register(mux *httprouter.Router, upg *websocket.Upgrader) {
	api := provider{
		upg: upg,
	}
	mux.GET("/trace", api.handleTrace)
}

type provider struct {
	upg *websocket.Upgrader
}

type Response struct {
	Status string `json:"status"`
}

func (pr provider) handleTrace(w http.ResponseWriter, r *http.Request, p httprouter.Params) {
	upgrade, err := pr.upg.Upgrade(w, r, nil)
	if err != nil {
		log.Println(upgrade)
		return
	}
	defer upgrade.Close()
	ticker := time.NewTicker(5 * time.Second)
	defer ticker.Stop()
	for {
		select {
		case <-ticker.C:
			cmd := exec.Command("traceroute", "yss.su")
			var buf bytes.Buffer
			cmd.Stdout = &buf
			err = cmd.Run()
			var resp Response
			if err != nil {
				log.Println(err)
				resp = Response{
					Status: "YSS.SU is not awailable",
				}
			} else {
				resp = Response{
					Status: buf.String(),
				}
			}
			err = upgrade.WriteJSON(&resp)
			if err != nil {
				log.Println(err)
				return
			}
		}
	}
}

func main() {
	mux := httprouter.New()

	upgrade := &websocket.Upgrader{
		ReadBufferSize:  1024,
		WriteBufferSize: 1024,
		CheckOrigin: func(r *http.Request) bool {
			return true
		},
	}

	Register(mux, upgrade)

	log.Fatal(http.ListenAndServe("localhost:8383", mux))
}
