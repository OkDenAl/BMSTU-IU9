package kvtransaction

import (
	"context"
	"errors"
	"fmt"
	"sync"
)

var (
	ErrTxClosed   = errors.New("tx is closed")
	ErrTxConflict = errors.New("tx conflict")
)

type Transaction struct {
	id     string
	writes map[string]any
	c      *Cluster
	closed bool
	mu     sync.RWMutex
}

func newTransaction(id string, c *Cluster) *Transaction {
	return &Transaction{
		id:     id,
		writes: make(map[string]any),
		c:      c,
		closed: false,
		mu:     sync.RWMutex{},
	}
}

func (tx *Transaction) Commit(ctx context.Context) error {
	tx.mu.Lock()
	defer tx.mu.Unlock()
	if tx.closed {
		return ErrTxClosed
	}

	fmt.Println("TX", tx.id, "COMMIT START")

	nodesMsgs := map[*node][]any{}
	for k, v := range tx.writes {
		n := tx.c.findNodeByKey(k)
		nodesMsgs[n] = append(nodesMsgs[n], k, v)
	}

	responses := make([]chan bool, 0, len(nodesMsgs))
	for n, kv := range nodesMsgs {
		resp := make(chan bool)
		responses = append(responses, resp)
		n.txInbox <- newMessage(tx.id, prepare, kv, resp)
	}

	ok := true
	for _, r := range responses {
		select {
		case res := <-r:
			ok = ok && res
		case <-ctx.Done():
			ok = false
		}
	}

	if !ok {
		for n, kv := range nodesMsgs {
			n.txInbox <- newMessage(tx.id, rollback, kv, nil)
		}
		fmt.Println("TX", tx.id, "ABORT")
		tx.closed = true
		return ErrTxConflict
	}

	for n, kv := range nodesMsgs {
		n.txInbox <- newMessage(tx.id, commit, kv, nil)
	}

	fmt.Println("TX", tx.id, "COMMIT")

	tx.closed = true

	return nil
}

func (tx *Transaction) Rollback() error {
	tx.mu.Lock()
	defer tx.mu.Unlock()
	if tx.closed {
		return ErrTxClosed
	}

	fmt.Println("TX", tx.id, "ROLLBACK START")

	nodes := map[*node][]any{}
	for k, v := range tx.writes {
		n := tx.c.findNodeByKey(k)
		nodes[n] = append(nodes[n], k, v)
	}

	for n, writes := range nodes {
		n.txInbox <- newMessage(tx.id, rollback, writes, nil)
	}

	tx.closed = true

	fmt.Println("TX", tx.id, "ROLLBACK")
	return nil
}

func (tx *Transaction) Get(ctx context.Context, key string) (any, error) {
	tx.mu.Lock()
	defer tx.mu.Unlock()
	if tx.closed {
		return nil, ErrTxClosed
	}

	if v, ok := tx.writes[key]; ok {
		return v, nil
	}

	fmt.Println("TX", tx.id, "GET", key)
	return tx.c.findNodeByKey(key).get(ctx, key)
}

func (tx *Transaction) Set(key string, value any) error {
	tx.mu.Lock()
	defer tx.mu.Unlock()
	if tx.closed {
		return ErrTxClosed
	}
	tx.writes[key] = value

	fmt.Println("TX", tx.id, "SET", key, value)
	return nil
}
