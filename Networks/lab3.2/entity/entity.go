package entity

import (
	"encoding/json"
	"log"
	"net"
	"strings"
)

type Address struct {
	IpV4 string
	Port string
}

type Package struct {
	From string
	To   string
	Data Vote
}

type Vote struct {
	Title    string
	Creator  Address
	Variants map[string]int
}

func NewVote(msg []string, node *Node) Vote {
	var titleCloseInd int
	for i, word := range msg {
		if string(word[0]) == "1" {
			titleCloseInd = i
			break
		}
	}
	newVote := Vote{
		Title:    strings.Join(msg[:titleCloseInd], " "),
		Variants: make(map[string]int, 0),
		Creator:  node.Addr,
	}
	for _, variant := range msg[titleCloseInd:] {
		newVote.Variants[variant] = 0
	}
	return newVote
}

type Node struct {
	Connections map[string]bool
	Addr        Address
	IsVote      bool
	LastChoice  string
	CurVote     Vote
	Pkg         Package
}

func NewNode(thisAddr string, addresses []string) *Node {
	splited := strings.Split(thisAddr, ":")
	if len(splited) != 2 {
		return nil
	}
	curNode := &Node{
		Connections: make(map[string]bool),
		Addr: Address{
			IpV4: splited[0],
			Port: ":" + splited[1],
		}}
	curNode.ConnectTo(addresses)
	return curNode
}

func (n *Node) Run(handleServer func(*Node), handleClient func(*Node)) {
	go handleServer(n)
	handleClient(n)
}

func (n *Node) ConnectTo(addresses []string) {
	for _, addr := range addresses {
		n.Connections[addr] = true
	}
}

func (n *Node) WriteToServer(pkg *Package) {
	conn, err := net.Dial("tcp", pkg.To)
	if err != nil {
		log.Println(err)
		delete(n.Connections, pkg.To)
		return
	}
	jsonPkg, _ := json.Marshal(*pkg)
	defer conn.Close()
	conn.Write(jsonPkg)
}
