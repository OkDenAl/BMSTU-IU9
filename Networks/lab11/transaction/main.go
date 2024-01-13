package main

import (
	"context"
	"github.com/ethereum/go-ethereum/ethclient"
	"github.com/gorilla/websocket"
	"github.com/julienschmidt/httprouter"
	"lab11/databse"
	"log"
	"math/big"
	"net/http"
	"time"
)

func Register(mux *httprouter.Router, upg *websocket.Upgrader, db *databse.FirebaseDb) {
	api := provider{
		upg: upg,
		db:  db,
	}
	mux.GET("/transaction", api.handleGetByNum)
}

type Response struct {
	ChainId  string `json:"chainId"`
	Hash     string `json:"myHash"`
	Value    string `json:"myValue"`
	Cost     string `json:"cost"`
	To       string `json:"to"`
	Gas      uint64 `json:"gas"`
	GasPrise string `json:"gasPrise"`
}

type provider struct {
	upg *websocket.Upgrader
	db  *databse.FirebaseDb
}

func (pr *provider) handleGetByNum(w http.ResponseWriter, r *http.Request, p httprouter.Params) {
	upgrade, err := pr.upg.Upgrade(w, r, nil)
	if err != nil {
		log.Println(err)
		return
	}
	client, err := ethclient.Dial("https://mainnet.infura.io/v3/8133ff0c11dc491daac3f680d2f74d18")
	if err != nil {
		log.Println(err)
		return
	}
	blockNumber := big.NewInt(1500000)
	ticker := time.NewTicker(5 * time.Second)
	defer ticker.Stop()
	for {
		select {
		case <-ticker.C:
			block, err := client.BlockByNumber(context.Background(), blockNumber)
			if err != nil {
				log.Println(err)
			}
			responses := make([]Response, 0)
			for _, tx := range block.Transactions() {
				resp := Response{
					ChainId:  tx.ChainId().String(),
					Hash:     tx.Hash().String(),
					Value:    tx.Value().String(),
					Cost:     tx.Cost().String(),
					To:       tx.To().String(),
					Gas:      tx.Gas(),
					GasPrise: tx.GasPrice().String(),
				}
				responses = append(responses, resp)
			}
			err = pr.db.SaveData(context.Background(), "2 dashboard", responses)
			err = upgrade.WriteJSON(responses)
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
	db, err := databse.InitDb()
	if err != nil {
		log.Println(err)
	}

	Register(mux, upgrade, db)
	log.Fatal(http.ListenAndServe("localhost:8482", mux))
}
