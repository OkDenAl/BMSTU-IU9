package errors

import (
	"github.com/julienschmidt/httprouter"
	"lab9/async/pkg/logger"
	"net/http"
)

type Handle func(w http.ResponseWriter, r *http.Request, p httprouter.Params) error

func Middleware(next Handle, l logger.Logger) httprouter.Handle {
	return func(w http.ResponseWriter, r *http.Request, p httprouter.Params) {
		err := next(w, r, p)
		if err != nil {
			l.Info(err)
		}
	}
}
