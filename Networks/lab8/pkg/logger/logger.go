package logger

import (
	"log"
	"os"
)

type Logger interface {
	Info(a ...interface{})
	Infof(format string, a ...interface{})
	Debug(a ...interface{})
	Error(a ...interface{})
}

type logger struct {
	logInfo  *log.Logger
	logError *log.Logger
	isDebug  bool
}

func InitLog() *logger {
	return &logger{
		logInfo:  log.New(os.Stdout, "INFO:\t", log.Ldate|log.Ltime),
		logError: log.New(os.Stderr, "ERROR:\t", log.Ldate|log.Ltime),
		isDebug:  true,
	}
}
func (l *logger) Infof(format string, a ...interface{}) {
	l.logInfo.Printf(format, a...)
}

func (l *logger) Info(a ...interface{}) {
	l.logInfo.Println(a...)
}

func (l *logger) Debug(a ...interface{}) {
	if l.isDebug {
		l.logInfo.Println(a...)
	}
}

func (l *logger) Error(a ...interface{}) {
	l.logError.Fatal(a...)
}
