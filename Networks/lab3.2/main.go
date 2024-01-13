package main

import (
	"bufio"
	"encoding/json"
	"fmt"
	"lab3.2/entity"
	"log"
	"net"
	"os"
	"strings"
)

func InputString() string {
	fmt.Println("enter the command")
	msg, _ := bufio.NewReader(os.Stdin).ReadString('\n')
	return strings.Replace(msg, "\n", "", -1)
}

func handleServer(node *entity.Node) {
	listen, err := net.Listen("tcp", "0.0.0.0"+node.Addr.Port)
	if err != nil {
		log.Fatal(err)
	}
	defer listen.Close()
	for {
		conn, err := listen.Accept()
		if err != nil {
			break
		}
		go handleConnection(node, conn)
	}
}

func handleConnection(node *entity.Node, conn net.Conn) {
	defer conn.Close()
	var (
		buffer = make([]byte, 512)
		msg    string
		pack   entity.Package
	)
	for {
		length, err := conn.Read(buffer)
		if err != nil {
			break
		}
		msg += string(buffer[:length])
	}
	err := json.Unmarshal([]byte(msg), &pack)
	if err != nil {
		log.Fatal(err)
	}
	node.ConnectTo([]string{pack.From})
	if len(node.CurVote.Variants) == 0 {
		node.CurVote = pack.Data
	} else {
		for variant := range node.CurVote.Variants {
			node.CurVote.Variants[variant] += pack.Data.Variants[variant]
		}
	}
	if node.Addr == pack.Data.Creator {
		fmt.Printf("Пир с адресом %s сделал свой выбор\n", pack.From)
	} else {
		printVote(pack.Data)
	}
}
func handleClient(node *entity.Node) {
	for {
		message := InputString()
		splited := strings.Split(message, " ")
		switch splited[0] {
		case "/exit":
			log.Println("the program was exited")
			os.Exit(0)
		case "/connect":
			if len(splited) < 2 {
				log.Println("no connections hosts")
			} else {
				node.ConnectTo(splited[1:])
				log.Println("successfully connected")
			}
		case "/connections":
			printConnections(node)
		case "/vote":
			if len(splited[1:]) < 3 {
				log.Println("incorrect vote")
			} else {
				handleVote(node, splited[1:])
				log.Println("vote successfully created")
			}
		case "/answer":
			if node.CurVote.Creator == node.Addr {
				log.Println("u cant vote in this vote")
			} else {
				handleAnswer(node, splited[1])
			}
		case "/res":
			printRes(node.CurVote)
		default:
			log.Println("wrong command")
		}
	}
}

func handleAnswer(node *entity.Node, variant string) {
	f := false
	for a := range node.CurVote.Variants {
		if a == variant {
			f = true
			break
		}
	}
	if !f {
		log.Println("you chose incorrect variant")
		return
	}
	if node.LastChoice != variant {
		if node.IsVote {
			if node.CurVote.Variants[node.LastChoice] < 0 {
				node.CurVote.Variants[node.LastChoice] = 0
			} else {
				if node.CurVote.Variants[node.LastChoice] == 1 {
					node.CurVote.Variants[node.LastChoice] -= 2
				} else {
					node.CurVote.Variants[node.LastChoice] -= 1
				}
			}
		}
		node.CurVote.Variants[variant]++
		node.LastChoice = variant
	} else {
		for a := range node.CurVote.Variants {
			node.CurVote.Variants[a] = 0
		}
	}
	node.IsVote = true
	node.Pkg = entity.Package{
		Data: node.CurVote,
		From: node.Addr.IpV4 + node.Addr.Port,
		To:   node.CurVote.Creator.IpV4 + node.CurVote.Creator.Port,
	}
	node.WriteToServer(&node.Pkg)
}

func handleVote(node *entity.Node, msg []string) {
	vote := entity.NewVote(msg, node)
	node.CurVote = vote
	node.Pkg = entity.Package{
		Data: vote,
		From: node.Addr.IpV4 + node.Addr.Port,
	}
	for addr := range node.Connections {
		node.Pkg.To = addr
		node.WriteToServer(&node.Pkg)
	}
}

func main() {
	if len(os.Args) < 2 {
		log.Fatal("please specify the user port")
	}
	entity.NewNode(os.Args[1], os.Args[2:]).Run(handleServer, handleClient)
}
