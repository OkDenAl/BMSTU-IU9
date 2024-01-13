package main

import (
	"context"
	"fmt"
	"github.com/ethereum/go-ethereum/ethclient"
	"log"
	"math/big"
)

/*var client *db.Client

func init() {
	ctx := context.Background()
	conf := &firebase.Config{
		DatabaseURL: "https://lab11-2dd9e-default-rtdb.firebaseio.com/",
	}
	app, err := firebase.NewApp(ctx, conf)
	if err != nil {
		log.Fatalf("firebase.NewApp: %v", err)
	}
	client, err = app.Database(ctx)
	if err != nil {
		log.Fatalf("app.Firestore: %v", err)
	}
}

type message struct {
	Hello string `json:"hello"`
}*/

func main() {
	ethClient, err := ethclient.Dial("https://mainnet.infura.io/v3/e0dfd650398443718f5f88cddb826562")
	if err != nil {
		log.Fatalln(err)
	}
	header, err := ethClient.HeaderByNumber(context.Background(), nil)
	if err != nil {
		log.Fatal(err)
	}
	blockNumber := big.NewInt(header.Number.Int64())
	block, err := ethClient.BlockByNumber(context.Background(), blockNumber) //get block with this number
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println(block.Number().Uint64())
	fmt.Println(block.Time())
	fmt.Println(block.Difficulty().Uint64())
	fmt.Println(block.Hash().Hex())
	fmt.Println(string(len(block.Transactions())))
}
