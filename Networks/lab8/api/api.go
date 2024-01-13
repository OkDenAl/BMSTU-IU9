package api

import (
	"encoding/json"
	"github.com/julienschmidt/httprouter"
	"lab8/api/middleware"
	"lab8/api/protocol"
	apperrors "lab8/pkg/error"
	"lab8/pkg/logger"
	"lab8/service"
	"net/http"
)

func Register(router *httprouter.Router, serv *service.FileService, l logger.Logger) {
	app := &provider{
		fileService: serv,
		l:           l,
	}
	router.POST("/files/*path", middleware.LogMiddleware(middleware.ErrorMiddleware(app.HandlePost, l), l))
	router.GET("/files/*path", middleware.LogMiddleware(middleware.ErrorMiddleware(app.HandleGet, l), l))
}

type provider struct {
	fileService *service.FileService
	l           logger.Logger
}

func (a provider) HandlePost(w http.ResponseWriter, r *http.Request, p httprouter.Params) error {
	path := p.ByName("path")

	err := r.ParseMultipartForm(32 << 20)
	if err != nil {
		return err
	}
	args := make([]string, 0)
	for _, val := range r.Form {
		args = append(args, val[0])
	}
	a.l.Debug(args)

	buffer, status, err := a.fileService.ServeFile(path, args)
	if err != nil {
		return err
	}
	if !status.IsCpp {
		w.Write(buffer.Bytes())
		return nil
	}
	ans := buffer.String()

	resp := &protocol.Response{
		Answer: &ans,
		Err:    nil,
	}

	w.Header().Set("Content-Type", "application/json")
	err = json.NewEncoder(w).Encode(resp)
	if err != nil {
		return apperrors.ErrBodyEncode
	}
	return nil
}

func (a provider) HandleGet(w http.ResponseWriter, r *http.Request, p httprouter.Params) error {
	path := p.ByName("path")

	values := r.URL.Query()
	args := make([]string, 0)
	for _, val := range values {
		args = append(args, val[0])
	}
	buffer, status, err := a.fileService.ServeFile(path, args)
	if err != nil {
		return err
	}
	if !status.IsCpp {
		w.Write(buffer.Bytes())
		return nil
	}
	ans := buffer.String()
	resp := &protocol.Response{
		Answer: &ans,
		Err:    nil,
	}
	w.Header().Set("Content-Type", "application/json")
	err = json.NewEncoder(w).Encode(resp)
	if err != nil {
		return apperrors.ErrBodyEncode
	}
	return nil
}
