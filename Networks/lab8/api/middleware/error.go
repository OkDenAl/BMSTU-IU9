package middleware

import (
	"encoding/json"
	"github.com/julienschmidt/httprouter"
	"lab8/api/protocol"
	apperrors "lab8/pkg/error"
	"lab8/pkg/logger"
	"net/http"
)

type Handle func(http.ResponseWriter, *http.Request, httprouter.Params) error

func ErrorMiddleware(next Handle, l logger.Logger) httprouter.Handle {
	return func(w http.ResponseWriter, r *http.Request, p httprouter.Params) {
		defer func() {
			err := recover()
			if err != nil {
				l.Info(err)
			}
		}()
		err := next(w, r, p)
		l.Info(err)
		if err != nil {
			code := respondError(w, err)
			if code == http.StatusInternalServerError {
				l.Infof("unexpected error (%s)", err)
			}
		}
	}
}

func respondError(w http.ResponseWriter, err error) int {
	w.Header().Set("Content-Type", "application/json")
	var (
		errorResp protocol.Error
		code      int
	)
	switch CurErr := err.(type) {
	case *apperrors.AppError:
		errorResp.Message = CurErr.Error()
		code = http.StatusBadRequest
		/*errorResp.Code = code*/
	default:
		errorResp.Message = "unexpected internal server error"
		code = http.StatusInternalServerError
		//errorResp.Code = &code
	}

	w.WriteHeader(code)
	resp := protocol.Response{
		Answer: nil,
		Err:    &errorResp,
	}
	err = json.NewEncoder(w).Encode(&resp)
	if err != nil {
		w.WriteHeader(http.StatusInternalServerError)
	}
	return code
}
