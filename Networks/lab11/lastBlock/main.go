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
	"strconv"
	"time"
)

func Register(mux *httprouter.Router, upg *websocket.Upgrader, db *databse.FirebaseDb) {
	api := provider{
		upg: upg,
		db:  db,
	}
	mux.GET("/getLast", api.handleGetLast)
}

type Response struct {
	BlockNumber       string `json:"blockNumber"`
	BlockTime         string `json:"blockTime"`
	BlockDifficulty   string `json:"blockDifficulty"`
	BlockHash         string `json:"blockHash"`
	BlockTransactions string `json:"blockTransactions"`
}

type provider struct {
	upg *websocket.Upgrader
	db  *databse.FirebaseDb
}

func (pr *provider) handleGetLast(w http.ResponseWriter, r *http.Request, p httprouter.Params) {
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
	ticker := time.NewTicker(5 * time.Second)
	defer ticker.Stop()
	for {
		select {
		case <-ticker.C:
			header, err := client.HeaderByNumber(context.Background(), nil)
			if err != nil {
				log.Fatal(err)
			}
			blockNumber := big.NewInt(header.Number.Int64())
			block, err := client.BlockByNumber(context.Background(), blockNumber)
			if err != nil {
				log.Println(err)
			}
			resp := &Response{
				BlockNumber:       block.Number().String(),
				BlockTime:         strconv.FormatUint(block.Time(), 10),
				BlockDifficulty:   block.Difficulty().String(),
				BlockHash:         block.Hash().String(),
				BlockTransactions: strconv.Itoa(len(block.Transactions())),
			}
			err = pr.db.SaveData(context.Background(), "3 dashboard", resp)
			if err != nil {
				log.Println(err)
				return
			}
			err = upgrade.WriteJSON(resp)
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
	log.Fatal(http.ListenAndServe("localhost:8483", mux))
}
