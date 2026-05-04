package main

import (
	"context"
	"log"
	"time"

	"github.com/pkg/errors"
	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/mongo"
	"go.mongodb.org/mongo-driver/mongo/options"
	"golang.org/x/sync/errgroup"
)

func testNonRepeatableRead(ctx context.Context, client *mongo.Client, txOpts ...*options.TransactionOptions) error {
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

	g, ctx := errgroup.WithContext(ctx)

	g.Go(func() error {
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

		var result1 bson.M
		if err = coll.FindOne(sessCtx1, bson.M{"_id": 1}).Decode(&result1); err != nil {
			return errors.Wrap(err, "failed to read in transaction1")
		}
		initialBalance1 := result1["balance"]
		log.Printf("Транзакция 1: Начальный баланс по индексу 1 = %v\n", initialBalance1)

		if _, err = coll.UpdateOne(sessCtx1, bson.M{"_id": 1}, bson.M{"$inc": bson.M{"balance": 50}}); err != nil {
			return errors.Wrap(err, "failed to update in transaction1")
		}
		log.Println("Транзакция 1: Обновление по индексу 1 завершено: 100 -> 150")

		if err = session1.CommitTransaction(sessCtx1); err != nil {
			return errors.Wrap(err, "failed to commit transaction1")
		}
		log.Println("Транзакция 1: Зафиксирована")
		return nil
	})

	g.Go(func() error {
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

		var result2 bson.M
		if err = coll.FindOne(sessCtx2, bson.M{"_id": 1}).Decode(&result2); err != nil {
			return errors.Wrap(err, "failed to read in transaction2 (first time)")
		}
		balance1Read1 := result2["balance"]
		log.Printf("Транзакция 2: Баланс после первого чтения по индексу 1 = %v\n", balance1Read1)

		time.Sleep(50 * time.Millisecond)

		if err = coll.FindOne(sessCtx2, bson.M{"_id": 1}).Decode(&result2); err != nil {
			return errors.Wrap(err, "failed to read in transaction2 (second time)")
		}
		balance1Read2 := result2["balance"]
		log.Printf("Транзакция 2: Баланс после второго чтения по индексу 1 = %v\n", balance1Read2)

		if balance1Read1 != balance1Read2 {
			log.Println("Non Repeatable Read наблюдается")
		} else {
			log.Println("Non Repeatable Read не наблюдается")
		}

		if err = session2.CommitTransaction(sessCtx2); err != nil {
			return errors.Wrap(err, "failed to commit transaction2")
		}
		log.Println("Транзакция 2: Зафиксирована\n")
		return nil
	})

	if err := g.Wait(); err != nil {
		return errors.Wrap(err, "error running transactions")
	}

	return nil
}
