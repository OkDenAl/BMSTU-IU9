package main

import (
	"encoding/json"
	"github.com/gorilla/websocket"
	"github.com/jlaffaye/ftp"
	"github.com/julienschmidt/httprouter"
	"log"
	"net/http"
	"time"
)

func Register(mux *httprouter.Router, upg *websocket.Upgrader, client *ftp.ServerConn) {
	api := provider{
		upg:    upg,
		client: client,
	}
	mux.GET("/ftp", api.handleFTP)
}

type Response struct {
	Out string `json:"out"`
	Err string `json:"err"`
}

type provider struct {
	upg    *websocket.Upgrader
	client *ftp.ServerConn
}

func (pr *provider) handleFTP(w http.ResponseWriter, r *http.Request, p httprouter.Params) {
	upgrade, err := pr.upg.Upgrade(w, r, nil)
	if err != nil {
		log.Println(err)
		return
	}
	defer upgrade.Close()
	ticker := time.NewTicker(time.Second * 5)
	defer ticker.Stop()
	for {
		select {
		case <-ticker.C:
			f, err := pr.client.Retr("achtung.txt")
			var out, errorStr string
			if err != nil {
				errorStr = "no file"
			}
			if err == nil {
				buf := make([]byte, 10)
				for {
					bytesRead, _ := f.Read(buf)
					if bytesRead == 0 {
						break
					}
					out += string(buf)
				}
				f.Close()
			}
			resp := &Response{
				Out: out,
				Err: errorStr,
			}
			jsonResp, err := json.Marshal(resp)
			if err != nil {
				log.Println(err)
				return
			}
			err = upgrade.WriteMessage(1, jsonResp)
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

	client, err := ftp.Dial("students.yss.su:21")
	if err != nil {
		log.Fatal(err)
	}
	err = client.Login("ftpiu8", "3Ru7yOTA")
	if err != nil {
		log.Fatal(err)
	}

	Register(mux, upgrade, client)

	log.Fatal(http.ListenAndServe("151.248.113.144:8181", mux))
}
