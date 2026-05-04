package main

import (
	"context"
	"log"

	"github.com/pkg/errors"
	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/mongo"
	"go.mongodb.org/mongo-driver/mongo/options"
)

func testDirtyRead(ctx context.Context, client *mongo.Client, txOpts ...*options.TransactionOptions) error {
	coll := client.Database("test1").Collection("accounts")
	if _, err := coll.DeleteMany(ctx, bson.M{}); err != nil {
		return errors.Wrap(err, "failed to clear collection")
	}

	if _, err := coll.InsertMany(ctx, []interface{}{
		bson.M{"_id": 1, "balance": 100},
		bson.M{"_id": 2, "balance": 100},
	}); err != nil {
		return errors.Wrap(err, "failed to insert initial data")
	}

	log.Println("Транзакция 1 начинается")
	session1, err := client.StartSession()
	if err != nil {
		return errors.Wrap(err, "failed to start session1")
	}
	defer session1.EndSession(ctx)

	if err = session1.StartTransaction(txOpts...); err != nil {
		return errors.Wrap(err, "failed to start transaction1")
	}
	sessCtx1 := mongo.NewSessionContext(ctx, session1)
	defer func() {
		log.Println("Транзакция 1: Откатываем\n")
		session1.AbortTransaction(sessCtx1)
	}()

	log.Println("Транзакция 1: Обновляем данные в 1 транзакции: 100 -> 150 по индексу 1")
	if _, err = coll.UpdateOne(sessCtx1, bson.M{"_id": 1}, bson.M{"$inc": bson.M{"balance": 50}}); err != nil {
		return errors.Wrap(err, "failed to update in transaction1")
	}

	log.Println("Транзакция 2 начинается")
	session2, err := client.StartSession()
	if err != nil {
		return errors.Wrap(err, "failed to start session2")
	}
	defer session2.EndSession(ctx)

	if err = session2.StartTransaction(txOpts...); err != nil {
		return errors.Wrap(err, "failed to start transaction2")
	}
	sessCtx2 := mongo.NewSessionContext(ctx, session2)
	defer func() {
		log.Println("Транзакция 2: Откатываем")
		session2.AbortTransaction(sessCtx2)
	}()

	log.Println("Транзакция 2: Получаем данные в транзакции 2 по индексу 1")
	var result bson.M
	if err = coll.FindOne(sessCtx2, bson.M{"_id": 1}).Decode(&result); err != nil {
		return errors.Wrap(err, "Транзакция 2: failed to read in transaction2")
	}
	log.Printf("Транзакция 2: Видит balance = %v\n", result["balance"])

	if result["balance"].(int32) == 100 {
		log.Println("Dirty Read не наблюдается")
	} else {
		log.Println("Dirty Read наблюдается")
	}

	return nil
}
