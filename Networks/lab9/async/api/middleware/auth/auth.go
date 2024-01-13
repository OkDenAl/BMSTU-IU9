package auth

import (
	"errors"
	"fmt"
	"github.com/julienschmidt/httprouter"
	error2 "lab9/async/api/middleware/errors"
	"lab9/async/pkg/utils"
	"net/http"
	"strconv"
	"strings"
)

func Middleware(next httprouter.Handle) error2.Handle {
	return func(w http.ResponseWriter, r *http.Request, p httprouter.Params) error {
		authHeader := r.Header.Get("Authorization")
		if authHeader == "" {
			return errors.New("nil authHeader")
		}
		accessToken := strings.Split(authHeader, " ")[1]
		id, _ := strconv.Atoi(p.ByName("userId"))
		isTokenValid := utils.ValidateAccessToken(accessToken, id)
		if !isTokenValid {
			return fmt.Errorf("%w: %v", "ошибка аутентификации", "invalid access token")
		}
		next(w, r, p)
		return nil
	}
}
