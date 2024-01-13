package logging

import (
	"github.com/julienschmidt/httprouter"
	"lab9/async/pkg/logger"
	"net/http"
)

func Middleware(handle httprouter.Handle, l logger.Logger) httprouter.Handle {
	return func(w http.ResponseWriter, r *http.Request, p httprouter.Params) {
		l.Info(r.Method, r.URL, r.Proto)
		l.Debug("\n\tHeader:\n\t", r.Header, "\n\tBody:\n\t", r.Body)
		handle(w, r, p)
	}
}
