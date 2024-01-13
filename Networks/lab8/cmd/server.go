package main

import (
	"github.com/jlaffaye/ftp"
	"github.com/julienschmidt/httprouter"
	"lab8/api"
	"lab8/config"
	"lab8/ftpServer"
	"lab8/pkg/logger"
	"lab8/service"
	"log"
	"net/http"
)

func connectToFtp(cfg *config.Config) (*ftp.ServerConn, error) {
	client, err := ftp.Dial("localhost:" + cfg.FtpServer.Port)
	if err != nil {
		return nil, err
	}
	err = client.Login(cfg.FtpServer.User, cfg.FtpServer.Password)
	if err != nil {
		return nil, err
	}
	return client, nil
}

func main() {
	cfg, err := config.InitConfig()
	if err != nil {
		panic("cant init config")
	}
	l := logger.InitLog()
	ftpServer.StartFTP()
	client, err := connectToFtp(cfg)
	if err != nil {
		l.Debug(err)
		panic("can't connect to ftp server")
	}
	mux := httprouter.New()
	api.Register(mux, service.NewFileService(client), l)
	l.Infof("starting a server on the %s port", cfg.FtpServer.Port)
	log.Fatal(http.ListenAndServe(":"+cfg.HttpServer.Port, mux))
}
