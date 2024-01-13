package main

import (
	"fmt"
	"github.com/julienschmidt/httprouter"
	"lab9/async/api/handlers/auth"
	"lab9/async/api/handlers/websocket"
	"lab9/async/config"
	"lab9/async/pkg/database"
	"lab9/async/pkg/logger"
	"lab9/async/repository"
	"lab9/async/service"
	"net/http"
)

func main() {
	cfg, err := config.InitConfig()
	if err != nil {
		fmt.Println(err)
		panic("cant parse config")
	}
	l := logger.InitLog()
	db, err := database.New(cfg, l)
	if err != nil {
		l.Debug(err)
		panic("cant connect to database")
	}
	mux := httprouter.New()
	userService := service.NewUserService(repository.NewUserRepository(db))
	websocket.Register(mux, l, userService)
	auth.Register(mux, l, userService)

	l.Info("starting server on " + cfg.Server.Host + ":" + cfg.Server.Port)
	l.Error(http.ListenAndServe(":"+cfg.Server.Port, mux))
}
