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

func testWriteSkew(parentCtx context.Context, client *mongo.Client, txOpts ...*options.TransactionOptions) error {
	coll := client.Database("test1").Collection("doctors")
	if _, err := coll.DeleteMany(parentCtx, bson.M{}); err != nil {
		return errors.Wrap(err, "failed to clear collection")
	}

	if _, err := coll.InsertMany(parentCtx, []interface{}{
		bson.M{"_id": "doctor1", "on_call": true},
		bson.M{"_id": "doctor2", "on_call": true},
	}); err != nil {
		return errors.Wrap(err, "failed to insert initial doctors")
	}

	g, ctx := errgroup.WithContext(parentCtx)
	g.Go(func() error {
		log.Println("Транзакция 1 начинается")
		session1, err := client.StartSession()
		if err != nil {
			return errors.Wrap(err, "failed to start session 1")
		}
		defer session1.EndSession(ctx)

		if err = session1.StartTransaction(txOpts...); err != nil {
			return errors.Wrap(err, "failed to start transaction 1")
		}
		sessCtx1 := mongo.NewSessionContext(ctx, session1)

		count, err := coll.CountDocuments(sessCtx1, bson.M{"on_call": true})
		if err != nil {
			return errors.Wrap(err, "failed to count on_call doctors")
		}
		log.Printf("Транзакция 1: on_call count = %d\n", count)

		if count > 1 {
			time.Sleep(100 * time.Millisecond)
			if _, err = coll.UpdateOne(
				sessCtx1,
				bson.M{"_id": "doctor1"},
				bson.M{"$set": bson.M{"on_call": false}},
			); err != nil {
				return errors.Wrap(err, "failed to update doctor1")
			}
			log.Println("Транзакция 1: doctor1 выключен")
		}

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
			return errors.Wrap(err, "failed to start session 2")
		}
		defer session2.EndSession(ctx)

		if err = session2.StartTransaction(txOpts...); err != nil {
			return errors.Wrap(err, "failed to start transaction 2")
		}

		sessCtx2 := mongo.NewSessionContext(ctx, session2)

		count, err := coll.CountDocuments(sessCtx2, bson.M{"on_call": true})
		if err != nil {
			return errors.Wrap(err, "failed to count on_call doctors")
		}
		log.Printf("Транзакция 2: on_call count = %d\n", count)

		if count > 1 {
			time.Sleep(100 * time.Millisecond)
			if _, err = coll.UpdateOne(
				sessCtx2,
				bson.M{"_id": "doctor2"},
				bson.M{"$set": bson.M{"on_call": false}},
			); err != nil {
				return errors.Wrap(err, "failed to update doctor2")
			}
			log.Println("Транзакция 2: doctor2 выключен")
		}

		if err = session2.CommitTransaction(sessCtx2); err != nil {
			return errors.Wrap(err, "failed to commit transaction 2")
		}

		log.Println("Транзакция 2: Зафиксирована")
		return nil
	})

	if err := g.Wait(); err != nil {
		return errors.Wrap(err, "error running write skew test")
	}

	countFinal, err := coll.CountDocuments(parentCtx, bson.M{"on_call": true})
	if err != nil {
		return errors.Wrap(err, "failed to final count on_call doctors")
	}

	log.Printf("Финальное количество on_call = %d\n", countFinal)
	if countFinal == 0 {
		log.Println("Write skew произошёл — инвариант нарушен\n")
	} else {
		log.Println("Write skew не произошёл\n")
	}

	return nil
}
