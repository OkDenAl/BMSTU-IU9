package databse

import (
	"context"
	firebase "firebase.google.com/go"
	"firebase.google.com/go/db"
	"fmt"
	"google.golang.org/api/option"
	"log"
)

type FirebaseDb struct {
	client *db.Client
}

func InitDb() (*FirebaseDb, error) {
	ctx := context.Background()
	opt := option.WithCredentialsFile("Cred.json")
	conf := &firebase.Config{
		DatabaseURL: "https://lab11-2dd9e-default-rtdb.firebaseio.com/",
	}
	app, err := firebase.NewApp(ctx, conf, opt)
	if err != nil {
		fmt.Errorf("error initializing app: %v", err)
	}
	client, err := app.Database(ctx)
	if err != nil {
		log.Fatalf("app.Firestore: %v", err)
	}
	return &FirebaseDb{client: client}, err
}

func (db *FirebaseDb) SaveData(ctx context.Context, path string, resp interface{}) error {
	return db.client.NewRef(path).Set(ctx, resp)
}
