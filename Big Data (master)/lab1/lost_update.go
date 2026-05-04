package main

import (
	"context"
	"log"
	"strings"
	"time"

	"github.com/pkg/errors"
	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/mongo"
	"go.mongodb.org/mongo-driver/mongo/options"
	"golang.org/x/sync/errgroup"
)

func testLostUpdate(parentCtx context.Context, client *mongo.Client, txOpts ...*options.TransactionOptions) error {
	coll := client.Database("test1").Collection("accounts")
	if _, err := coll.DeleteMany(parentCtx, bson.M{}); err != nil {
		return errors.Wrap(err, "failed to clear collection")
	}

	if _, err := coll.InsertOne(parentCtx, bson.M{"_id": 1, "balance": 100}); err != nil {
		return errors.Wrap(err, "failed to insert initial document")
	}

	g, ctx := errgroup.WithContext(parentCtx)
	g.Go(func() error {
		log.Println("Транзакция 1 начинается")
		session1, err := client.StartSession()
		if err != nil {
			return err
		}
		defer session1.EndSession(ctx)

		if err = session1.StartTransaction(txOpts...); err != nil {
			return errors.Wrap(err, "failed to start transaction 1")
		}
		sessCtx1 := mongo.NewSessionContext(ctx, session1)

		var doc bson.M
		if err = coll.FindOne(sessCtx1, bson.M{"_id": 1}).Decode(&doc); err != nil {
			return errors.Wrap(err, "failed to find document")
		}

		balance := doc["balance"].(int32)
		newBalance := balance + 50
		time.Sleep(100 * time.Millisecond)

		if _, err = coll.UpdateOne(
			sessCtx1,
			bson.M{"_id": 1},
			bson.M{"$set": bson.M{"balance": newBalance}},
		); err != nil {
			return errors.Wrap(err, "failed to update balance")
		}
		log.Printf("Транзакция 1: Обновляет баланс до %d\n", newBalance)

		if err = session1.CommitTransaction(sessCtx1); err != nil {
			return errors.Wrap(err, "failed to commit transaction 1")
		}

		log.Println("Транзакция 1: Зафиксирована")
		return nil
	})

	g.Go(func() error {
		time.Sleep(20 * time.Millisecond)
		log.Println("Транзакция 2 начинается")
		session2, err := client.StartSession()
		if err != nil {
			return err
		}
		defer session2.EndSession(ctx)

		if err = session2.StartTransaction(txOpts...); err != nil {
			return err
		}
		sessCtx2 := mongo.NewSessionContext(ctx, session2)

		var doc bson.M
		if err = coll.FindOne(sessCtx2, bson.M{"_id": 1}).Decode(&doc); err != nil {
			return errors.Wrap(err, "failed to find document")
		}

		balance := doc["balance"].(int32)
		newBalance := balance + 50
		time.Sleep(100 * time.Millisecond)

		if _, err = coll.UpdateOne(
			sessCtx2,
			bson.M{"_id": 1},
			bson.M{"$set": bson.M{"balance": newBalance}},
		); err != nil {
			return errors.Wrap(err, "failed to update balance")
		}
		log.Printf("Транзакция 2: Обновляет баланс до %d\n", newBalance)

		if err = session2.CommitTransaction(sessCtx2); err != nil {
			return errors.Wrap(err, "failed to commit transaction 2")
		}

		log.Println("Транзакция 2: Зафиксирована")
		return nil
	})

	if err := g.Wait(); err != nil {
		log.Println("Одна из транзакций завершилась с ошибкой:", err)
		if strings.Contains(err.Error(), "WriteConflict") {
			log.Println("Lost Update не наблюдается")
		} else {
			log.Println("Lost Update наблюдается")
		}
	}

	var finalDoc bson.M
	if err := coll.FindOne(parentCtx, bson.M{"_id": 1}).Decode(&finalDoc); err != nil {
		return errors.Wrap(err, "failed to find final document")
	}

	log.Printf("Финальный баланс = %v\n\n", finalDoc["balance"])
	return nil
}
