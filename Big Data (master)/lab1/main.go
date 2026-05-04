package main

import (
	"context"
	"log"

	"go.mongodb.org/mongo-driver/mongo"
	"go.mongodb.org/mongo-driver/mongo/options"
	"go.mongodb.org/mongo-driver/mongo/readconcern"
	"go.mongodb.org/mongo-driver/mongo/writeconcern"
)

var txReadConcernsOpts = map[ReadConcern]*options.TransactionOptions{
	ReadConcernLocal:    options.Transaction().SetReadConcern(readconcern.Local()),
	ReadConcernMajority: options.Transaction().SetReadConcern(readconcern.Majority()),
	ReadConcernSnapshot: options.Transaction().SetReadConcern(readconcern.Snapshot()),
}

var txWriteConcernsOpts = map[WriteConcern]*options.TransactionOptions{
	WriteConcernOne:      options.Transaction().SetWriteConcern(writeconcern.New(writeconcern.W(1))),
	WriteConcernMajority: options.Transaction().SetWriteConcern(writeconcern.New(writeconcern.WMajority())),
}

func main() {
	ctx := context.Background()

	client, err := mongo.Connect(ctx, options.Client().ApplyURI("mongodb://user:password@mongo1:27017,mongo2:27017,mongo3:27017/test1??replicaSet=rs01"))
	if err != nil {
		log.Fatal(err)
	}
	defer client.Disconnect(ctx)

	log.Println("Dirty Read Test\n")
	if err = runTest(ctx, client, testDirtyRead); err != nil {
		log.Fatal(err)
	}

	log.Println("Non-Repeatable Read Test\n")
	if err = runTest(ctx, client, testNonRepeatableRead); err != nil {
		log.Fatal(err)
	}

	log.Println("Phantom Read Test\n")
	if err = runTest(ctx, client, testPhantomRead); err != nil {
		log.Fatal(err)
	}

	log.Println("Lost Update Test\n")
	if err = runTest(ctx, client, testLostUpdate); err != nil {
		log.Fatal(err)
	}

	log.Println("Write Skew Test\n")
	if err = runTest(ctx, client, testWriteSkew); err != nil {
		log.Fatal(err)
	}
}

func runTest(ctx context.Context, client *mongo.Client, test func(context.Context, *mongo.Client, ...*options.TransactionOptions) error) error {
	for name, txOpt := range txReadConcernsOpts {
		for nameWC, txOptWC := range txWriteConcernsOpts {
			log.Printf("Уровень согласованности чтения: %s, Уровень согласованности записи: %s\n", name, nameWC)
			if err := test(ctx, client, txOpt, txOptWC); err != nil {
				return err
			}
		}
	}

	return nil
}
