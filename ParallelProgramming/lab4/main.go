package main

import (
	"fmt"
	"math/rand"
	"sync"
	"time"
)

const (
	num     = 5
	minTime = 1000
	maxTime = 5000
)

func start() {
	var (
		output = make(chan string)
		forks  = make([]sync.Mutex, num)
	)

	for i := 0; i < num-1; i++ {
		go philosopher(i, forks, i, i+1, output)
	}
	go philosopher(4, forks, num-1, 0, output)

	t := time.After(10 * time.Second)
loop:
	for {
		select {
		case a := <-output:
			fmt.Println(a)
		case <-t:
			break loop
		}
	}
}

func philosopher(id int, forks []sync.Mutex, leftFork, rightFork int, output chan string) {
	for {
		var (
			thinkingTime = genTime()
			foodTime     = genTime()
			thinkAction  = "think"
			waitAction   = "wait"
		)

		output <- format("phil", id, thinkAction)
		time.Sleep(time.Duration(thinkingTime) * time.Millisecond)
		output <- format("phil", id, waitAction)
		if id == 4 {
			forks[rightFork].Lock()
			output <- format("fork", id, rightForkTookAction(rightFork))
			forks[leftFork].Lock()
			output <- format("fork", id, leftForkTookAction(leftFork))
		} else {
			forks[leftFork].Lock()
			output <- format("fork", id, leftForkTookAction(leftFork))
			forks[rightFork].Lock()
			output <- format("fork", id, rightForkTookAction(rightFork))
		}

		output <- format("phil", id, "eat")
		time.Sleep(time.Duration(foodTime) * time.Millisecond)
		output <- format("phil", id, waitAction)

		forks[rightFork].Unlock()
		output <- format("fork", id, rightForkBackAction(rightFork))
		forks[leftFork].Unlock()
		output <- format("fork", id, leftForkBackAction(leftFork))
	}
}

func genTime() int {
	return rand.Intn(maxTime+minTime) - minTime
}

func leftForkTookAction(forkId int) string {
	return fmt.Sprintf(`took left fork with id = %d`, forkId)
}

func rightForkTookAction(forkId int) string {
	return fmt.Sprintf(`took right fork with id = %d`, forkId)
}

func leftForkBackAction(forkId int) string {
	return fmt.Sprintf(`put left fork with id = %d`, forkId)
}

func rightForkBackAction(forkId int) string {
	return fmt.Sprintf(`put right fork with id = %d`, forkId)
}

func format(who string, id int, action string) string {
	return fmt.Sprintf("Philosopher: %d\t action: %s", id, action)
}

func main() {
	start()
}
