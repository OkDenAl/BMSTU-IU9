package main

import (
	"github.com/gliderlabs/ssh"
	"golang.org/x/crypto/ssh/terminal"
	"log"
	"os/exec"
	"strings"
)

func handler(sess ssh.Session) {
	term := terminal.NewTerminal(sess, "> ")
	for {
		line, err := term.ReadLine()
		if err != nil {
			break
		}
		splited := strings.Split(line, " ")
		//fmt.Println(splited)
		cmd := exec.Command(splited[0], splited[1:]...)
		cmd.Stdout = sess
		if err = cmd.Run(); err != nil {
			log.Println(err)
			return
		}
		if err != nil {
			log.Println(err)
			break
		}
	}
	log.Println("terminal closed")
}

func main() {
	server := ssh.Server{
		Addr:    ":2223",
		Handler: handler,
	}

	log.Println("starting ssh server on port 2223...")
	err := server.ListenAndServe()
	if err != nil {
		log.Fatal(err)
	}
}
