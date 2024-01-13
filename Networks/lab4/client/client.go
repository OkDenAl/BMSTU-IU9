package main

import (
	"bufio"
	"fmt"
	"golang.org/x/crypto/ssh"
	"io"
	"log"
	"os"
)

func sendRequest(cmd string, stdin io.WriteCloser) {
	if cmd == "/exit" {
		os.Exit(0)
	}

	/*command := strings.Split(cmd, " ")
	switch command[0] {
	case "/exit":
		os.Exit(0)
	case "/ping":
		cmd = "ping -c 1 " + "151.248.113.144"
	case "/getContent":
		cmd = "ls -a"
	case "/createDir":
		cmd = "mkdir " + strings.Join(command[1:], " ")
	case "/removeDir":
		cmd = "rmdir " + strings.Join(command[1:], " ")
	case "/getFile":
		cmd = "cat " + strings.Join(command[1:], " ")
	case "/GetPath":
		cmd = "pwd"
	default:
		log.Println("unknown command")
	}*/

	_, err := fmt.Fprintf(stdin, "%s\n", cmd)
	if err != nil {
		log.Fatal(err)
	}
}

func main() {
	config := &ssh.ClientConfig{
		User: "test",
		Auth: []ssh.AuthMethod{
			ssh.Password("SDHBCXdsedfs222"),
		},
		HostKeyCallback: ssh.InsecureIgnoreHostKey(),
	}
	client, err := ssh.Dial("tcp", ":2223", config)
	if err != nil {
		log.Fatal("Failed to dial: ", err)
	}
	defer client.Close()

	session, err := client.NewSession()
	if err != nil {
		log.Fatal("Failed to create session: ", err)
	}
	defer session.Close()

	stdin, err := session.StdinPipe()
	if err != nil {
		log.Fatal(err)
	}
	session.Stdout = os.Stdout
	session.Stderr = os.Stderr
	err = session.Shell()
	if err != nil {
		log.Fatal(err)
	}
	sc := bufio.NewScanner(os.Stdin)
	for sc.Scan() {
		cmd := sc.Text()
		sendRequest(cmd, stdin)
	}
}
