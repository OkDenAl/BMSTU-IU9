package main

import (
	"context"
	"fmt"
	"lab7/spamer/config"
	"lab7/spamer/pkg/database"
	"lab7/spamer/pkg/logger"
	mailer2 "lab7/spamer/repository/mailer"
	"lab7/spamer/service/mailer"
	"os"
	"os/signal"
)

func main() {
	cfg, err := config.InitConfig()
	if err != nil {
		panic(fmt.Errorf("unable to load config: %s", err))
	}
	log := logger.InitLog(cfg)
	log.Info("connecting to database")
	db, err := database.InitDb(cfg)
	if err != nil {
		panic(fmt.Errorf("unable to create new database: %s", err))
	}
	serv := mailer.NewMailService(mailer2.New(db))
	ctx, cancel := context.WithCancel(context.Background())
	gracefullShutdown(cancel, log)
	err = serv.SendEmail(ctx, log)
	if err != nil {
		log.Error(err)
	}
}

func gracefullShutdown(cancel context.CancelFunc, l logger.Logger) {
	done := make(chan os.Signal)
	signal.Notify(done, os.Interrupt)
	go func() {
		<-done
		l.Debug("canceling")
		cancel()
	}()
}
