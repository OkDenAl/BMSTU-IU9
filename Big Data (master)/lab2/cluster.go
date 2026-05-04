package kvtransaction

import (
	"context"
	"math/rand"
	"sync"
	"time"

	"github.com/google/uuid"
)

type Cluster struct {
	nodes []*node
	mu    sync.RWMutex
}

func NewCluster(ctx context.Context, size int) *Cluster {
	nodes := make([]*node, 0, size)
	for i := 0; i < size; i++ {
		nodes = append(nodes, newNode(ctx, i))
	}

	return &Cluster{
		nodes: nodes,
		mu:    sync.RWMutex{},
	}
}

func (c *Cluster) Get(ctx context.Context, key string) (any, error) {
	return c.findNodeByKey(key).get(ctx, key)
}

func (c *Cluster) Set(ctx context.Context, key string, value any) {
	c.findNodeByKey(key).set(ctx, key, value)
}

func (c *Cluster) Delete(ctx context.Context, key string) {
	c.findNodeByKey(key).delete(ctx, key)
}

func (c *Cluster) Begin() *Transaction {
	return newTransaction(uuid.New().String(), c)
}

func (c *Cluster) findNodeByKey(key string) *node {
	hash := 0
	for i, ch := range key {
		hash += int(ch) * (i + 1)
	}
	
	c.mu.RLock()
	defer c.mu.RUnlock()
	return c.nodes[hash%len(c.nodes)]
}

func networkDelay() {
	time.Sleep(time.Duration(rand.Intn(90)+10) * time.Millisecond)
}
