package middleware

import (
	"github.com/julienschmidt/httprouter"
	"lab8/pkg/logger"
	"net/http"
)

func LogMiddleware(handler httprouter.Handle, log logger.Logger) httprouter.Handle {
	return func(w http.ResponseWriter, r *http.Request, p httprouter.Params) {
		log.Info(r.Method, r.URL, r.Proto)
		log.Debug("\n\tHeader:\n\t", r.Header, "\n\tBody:\n\t", r.Body)
		handler(w, r, p)
	}
}
