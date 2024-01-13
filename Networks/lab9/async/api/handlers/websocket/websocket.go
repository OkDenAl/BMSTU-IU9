package websocket

import (
	"encoding/json"
	"github.com/gorilla/websocket"
	"github.com/julienschmidt/httprouter"
	"html/template"
	"lab9/async/api/middleware/auth"
	"lab9/async/api/middleware/errors"
	"lab9/async/api/middleware/logging"
	"lab9/async/pkg/logger"
	"lab9/async/service"
	"log"
	"net/http"
	"os/exec"
	"strings"
)

func Register(r *httprouter.Router, l logger.Logger, s service.UserService) {
	api := provider{
		l: l,
		upg: websocket.Upgrader{
			ReadBufferSize:  1024,
			WriteBufferSize: 1024,
		},
		s: s,
	}
	r.ServeFiles("/static/*filepath", http.Dir("static"))
	r.GET("/", logging.Middleware(api.StartPage, l))
	r.GET("/message/:userId", logging.Middleware(errors.Middleware(auth.Middleware(api.MessagePage), l), l))
	r.GET("/async", logging.Middleware(errors.Middleware(api.HandleAsync, l), l))
}

type provider struct {
	l   logger.Logger
	upg websocket.Upgrader
	s   service.UserService
}

func (pr provider) HandleAsync(w http.ResponseWriter, r *http.Request, p httprouter.Params) error {
	conn, err := pr.upg.Upgrade(w, r, nil)
	if err != nil {
		log.Print("upgrade failed: ", err)
		return err
	}
	defer conn.Close()
	for {
		_, message, err := conn.ReadMessage()
		parsedMessage := strings.Split(string(message), " ")
		b, err := exec.Command(parsedMessage[0], parsedMessage[1:]...).Output()
		if err != nil {
			pr.l.Info(err)
			break
		}
		res, err := json.Marshal(Answer{Text: string(b)})
		err = conn.WriteMessage(1, res)
		if err != nil {
			pr.l.Infof("write failed %v", err)
			break
		}
	}
	return nil
}

type Answer struct {
	Text string `json:"text"`
}

func (pr provider) StartPage(w http.ResponseWriter, _ *http.Request, _ httprouter.Params) {
	err := showHtml(w, "static/reg_page.html")
	if err != nil {
		pr.l.Error(err)
	}
}

func (pr provider) MessagePage(w http.ResponseWriter, _ *http.Request, _ httprouter.Params) {
	err := showHtml(w, "static/async.html")
	if err != nil {
		pr.l.Error(err)
	}
}

func showHtml(w http.ResponseWriter, path string) error {
	tmpl, err := template.ParseFiles(path)
	if err != nil {
		return err
	}
	return tmpl.Execute(w, nil)
}
