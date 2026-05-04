package kvtransaction

import (
	"context"
	"errors"
	"fmt"
	"sync"
)

var ErrNoRows = errors.New("no rows in result set")

type node struct {
	id   int
	data map[string]any
	mu   sync.RWMutex

	txInbox     chan *message
	lockManager *lockManager
}

func newNode(ctx context.Context, id int) *node {
	n := &node{
		id:          id,
		data:        map[string]any{},
		lockManager: newLockManager(),
		txInbox:     make(chan *message),
	}

	go n.processTxCmd(ctx)

	return n
}

func (n *node) processTxCmd(ctx context.Context) {
	for msg := range n.txInbox {
		networkDelay()
		switch msg.mType {
		case prepare:
			ok := true
			for i := 0; i < len(msg.kv); i += 2 {
				if !n.lockManager.tryLock(msg.txID, msg.kv[i].(string)) {
					ok = false
					break
				}
			}
			msg.resp <- ok
		case commit:
			for i := 0; i < len(msg.kv); i += 2 {
				key, value := msg.kv[i].(string), msg.kv[i+1]
				n.set(ctx, key, value)
				n.lockManager.unlock(msg.txID, msg.kv[i].(string))
			}
		case rollback:
			for i := 0; i < len(msg.kv); i += 2 {
				n.lockManager.unlock(msg.txID, msg.kv[i].(string))
			}
		}
	}
}

func (n *node) get(_ context.Context, key string) (any, error) {
	n.mu.RLock()
	defer n.mu.RUnlock()
	value, ok := n.data[key]
	if !ok {
		return nil, ErrNoRows
	}
	fmt.Printf("Node %d: Get %s: %v\n", n.id, key, value)
	return value, nil
}

func (n *node) set(_ context.Context, key string, value any) {
	n.mu.Lock()
	defer n.mu.Unlock()
	fmt.Printf("Node %d: Set %s: %v\n", n.id, key, value)
	n.data[key] = value
}

func (n *node) delete(_ context.Context, key string) {
	n.mu.Lock()
	defer n.mu.Unlock()
	fmt.Printf("Node %d: Delete %s\n", n.id, key)
	delete(n.data, key)
}
