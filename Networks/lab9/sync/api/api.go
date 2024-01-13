package api

import (
	"github.com/gorilla/websocket"
	"github.com/julienschmidt/httprouter"
	"html/template"
	"lab9/async/pkg/logger"
	"net/http"
	"path/filepath"
)

func Register(r *httprouter.Router, l logger.Logger, conn *websocket.Conn) {
	api := provider{
		l:    l,
		conn: conn,
	}
	r.ServeFiles("/static/*filepath", http.Dir("static"))
	r.GET("/", api.StartPage)
	r.POST("/sync", api.HandlePost)
}

type provider struct {
	l    logger.Logger
	conn *websocket.Conn
}

type Ans struct {
	Answer string
}

func (pr provider) HandlePost(w http.ResponseWriter, r *http.Request, p httprouter.Params) {
	err := r.ParseForm()
	if err != nil {
		pr.l.Error(err)
	}
	input := r.FormValue("text")
	err = pr.conn.WriteMessage(1, []byte(input))
	if err != nil {
		pr.l.Error(err)
	}
	_, msg, err := pr.conn.ReadMessage()
	if err != nil {
		pr.l.Error(err)
	}
	err = showHtml(w, Ans{
		Answer: string(msg),
	})
	if err != nil {
		pr.l.Error(err)
	}
}

func (pr provider) StartPage(w http.ResponseWriter, _ *http.Request, _ httprouter.Params) {
	err := showHtml(w, nil)
	if err != nil {
		pr.l.Error(err)
	}
}

func showHtml(w http.ResponseWriter, a interface{}) error {
	path := filepath.Join("static", "sync.html")
	tmpl, err := template.ParseFiles(path)
	if err != nil {
		return err
	}
	return tmpl.Execute(w, a)
}
