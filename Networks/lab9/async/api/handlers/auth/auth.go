package auth

import (
	"encoding/json"
	"fmt"
	"github.com/julienschmidt/httprouter"
	"lab9/async/api/middleware/errors"
	"lab9/async/api/middleware/logging"
	"lab9/async/entity"
	"lab9/async/pkg/logger"
	"lab9/async/service"
	"net/http"
	"time"
)

func Register(r *httprouter.Router, l logger.Logger, s service.UserService) {
	api := provider{
		l: l,
		s: s,
	}
	r.POST("/auth/register", logging.Middleware(errors.Middleware(api.HandleReg, l), l))
	r.POST("/auth/login", logging.Middleware(errors.Middleware(api.HandleLogin, l), l))
}

type provider struct {
	l logger.Logger
	s service.UserService
}

func (pr provider) HandleLogin(w http.ResponseWriter, r *http.Request, p httprouter.Params) error {
	err := r.ParseMultipartForm(32 << 20)
	if err != nil {
		pr.l.Error(err)
	}
	user := entity.User{
		Email:    r.FormValue("email"),
		Password: r.FormValue("password"),
	}
	pr.l.Debug(user)
	if err != nil {
		return err
	}
	userData, err := pr.s.Login(r.Context(), user)
	if err != nil {
		pr.l.Error(err)
	}
	http.SetCookie(w, &http.Cookie{
		Name:     "refreshToken",
		Value:    userData.Tokens.RefreshToken,
		Expires:  time.Now().Add(30 * 24 * time.Hour),
		HttpOnly: true,
	})
	w.Header().Set("Content-type", "application/json")
	json.NewEncoder(w).Encode(userData)
	return nil
}

func (pr provider) HandleReg(w http.ResponseWriter, r *http.Request, p httprouter.Params) error {
	err := r.ParseMultipartForm(32 << 20)
	if err != nil {
		pr.l.Error(err)
	}
	user := entity.User{
		Email:    r.FormValue("email"),
		Password: r.FormValue("password"),
	}
	fmt.Println(user)
	userData, err := pr.s.Registration(r.Context(), user)
	if err != nil {
		pr.l.Error(err)
	}
	http.SetCookie(w, &http.Cookie{
		Name:     "refreshToken",
		Value:    userData.Tokens.RefreshToken,
		Expires:  time.Now().Add(30 * 24 * time.Hour),
		HttpOnly: true,
	})
	w.Header().Set("Content-type", "application/json")
	return json.NewEncoder(w).Encode(userData)
}
