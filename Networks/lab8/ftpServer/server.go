package ftpServer

import (
	filedriver "github.com/goftp/file-driver"
	"github.com/goftp/server"
)

func StartFTP() {
	factory := &filedriver.FileDriverFactory{
		RootPath: "ftpServer",
		Perm:     server.NewSimplePerm("user", "group"),
	}

	opts := &server.ServerOpts{
		Factory:  factory,
		Port:     2121,
		Hostname: "localhost",
		Auth:     &server.SimpleAuth{Name: "admin", Password: "123456"},
	}
	serv := server.NewServer(opts)
	go serv.ListenAndServe()
}
