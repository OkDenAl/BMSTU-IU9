package main

type Message struct {
	isError bool
	text    string
}

func NewMessage(isError bool, text string) Message {
	return Message{isError: isError, text: text}
}
