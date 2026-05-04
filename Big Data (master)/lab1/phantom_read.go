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

func testPhantomRead(ctx context.Context, client *mongo.Client, txOpts ...*options.TransactionOptions) error {
	coll := client.Database("test1").Collection("accounts")
	if _, err := coll.DeleteMany(ctx, bson.M{}); err != nil {
		return errors.Wrap(err, "failed to clear collection")
	}

	if _, err := coll.InsertMany(ctx, []interface{}{
		bson.M{"_id": 1, "balance": 100},
		bson.M{"_id": 2, "balance": 200},
	}); err != nil {
		return errors.Wrap(err, "failed to insert initial data")
	}

	g, ctx := errgroup.WithContext(ctx)
	g.Go(func() error {
		time.Sleep(20 * time.Millisecond)
		log.Println("Транзакция 1 начинается")
		session1, err := client.StartSession()
		if err != nil {
			return err
		}
		defer session1.EndSession(ctx)

		if err = session1.StartTransaction(txOpts...); err != nil {
			return err
		}
		sessCtx1 := mongo.NewSessionContext(ctx, session1)

		if _, err = coll.InsertOne(sessCtx1, bson.M{"_id": 3, "balance": 150}); err != nil {
			return errors.Wrap(err, "failed to insert phantom row")
		}
		log.Println("Транзакция 1: Вставлена новая запись с balance=150")

		if err = session1.CommitTransaction(sessCtx1); err != nil {
			return err
		}

		log.Println("Транзакция 1: Зафиксирована")
		return nil
	})

	g.Go(func() error {
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

		filter := bson.M{"balance": bson.M{"$gte": 100}}

		cursor, err := coll.Find(sessCtx2, filter)
		if err != nil {
			return err
		}
		var results1 []bson.M
		if err = cursor.All(sessCtx2, &results1); err != nil {
			return err
		}
		log.Printf("Транзакция 2: Количество документов при первом чтении = %d\n", len(results1))

		time.Sleep(100 * time.Millisecond)
		cursor, err = coll.Find(sessCtx2, filter)
		if err != nil {
			return err
		}
		var results2 []bson.M
		if err = cursor.All(sessCtx2, &results2); err != nil {
			return err
		}
		log.Printf("Транзакция 2: Количество документов при втором чтении = %d\n", len(results2))

		if len(results1) != len(results2) {
			log.Println("Phantom read наблюдается")
		} else {
			log.Println("Phantom read не наблюдается")
		}

		if err = session2.CommitTransaction(sessCtx2); err != nil {
			return err
		}

		log.Println("Транзакция 2: Зафиксирована\n")
		return nil
	})

	if err := g.Wait(); err != nil {
		return errors.Wrap(err, "error running phantom read test")
	}

	return nil
}
