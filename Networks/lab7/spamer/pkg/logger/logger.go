package logger

import (
	"lab7/spamer/config"
	"log"
	"os"
)

type Logger interface {
	Info(a ...interface{})
	Debug(a ...interface{})
	Error(a ...interface{})
}

type logger struct {
	log     *log.Logger
	isDebug bool
}

func InitLog(cfg *config.Config) *logger {
	return &logger{
		log:     log.New(os.Stdout, "", log.Ldate|log.Ltime),
		isDebug: cfg.IsDebug,
	}
}

func (l *logger) Info(a ...interface{}) {
	l.log.Println(a...)
}

func (l *logger) Debug(a ...interface{}) {
	if l.isDebug {
		l.log.Println(a...)
	}
}

func (l *logger) Error(a ...interface{}) {
	l.log.Fatal(a...)
}
