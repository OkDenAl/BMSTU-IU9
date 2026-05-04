package main

import (
	"context"
	"fmt"
	"sync"
	"time"

	"kvtransaction"
)

func main() {
	ctx := context.Background()
	cluster := kvtransaction.NewCluster(ctx, 3)
	wg := sync.WaitGroup{}

	cluster.Set(ctx, "x", 100)

	// TX1
	wg.Add(1)
	go func() {
		defer wg.Done()
		tx := cluster.Begin()
		tx.Set("x", 1)
		tx.Set("y", 2)

		if err := tx.Commit(ctx); err != nil {
			fmt.Println("TX1 commit error:", err)
		}
	}()

	time.Sleep(20 * time.Millisecond)

	// TX2
	wg.Add(1)
	go func() {
		defer wg.Done()
		tx := cluster.Begin()
		tx.Set("y", 10)
		tx.Set("z", 20)
		if err := tx.Commit(ctx); err != nil {
			fmt.Println("TX2 commit error:", err)
		}
	}()

	wg.Add(1)
	go func() {
		defer wg.Done()
		tx := cluster.Begin()
		data, err := tx.Get(ctx, "x")
		if err != nil {
			return
		}
		fmt.Println("TX3 get x:", data)
		time.Sleep(70 * time.Millisecond)
		data, err = tx.Get(ctx, "x")
		if err != nil {
			fmt.Println("TX3 get error:", err)
			return
		}
		fmt.Println("TX3 get x:", data)
	}()

	wg.Wait()
	time.Sleep(100 * time.Millisecond)
}
