package main

import (
	"bufio"
	"fmt"
	"github.com/jlaffaye/ftp"
	"github.com/mmcdole/gofeed"
	"io"
	"log"
	"os"
	"strconv"
	"strings"
	"time"
)

const URL_FOR_PARSE = "https://news.google.com/atom?topic=t&hl=ru&gl=RU&ceid=RU:ru"

func handleCommand(cmd string, client *ftp.ServerConn) {
	split := strings.Split(cmd, " ")
	switch split[0] {
	case "/exit":
		os.Exit(0)
	case "ls":
		if len(split) < 2 {
			log.Println("bad command")
			return
		}
		entries, _ := client.List(split[1])
		for _, entry := range entries {
			name := entry.Name
			fmt.Println(name)
		}
	case "mdir":
		if len(split) < 2 {
			log.Println("bad command")
			return
		}
		err := client.MakeDir(split[1])
		if err != nil {
			log.Println(err)
		}
	case "rdir":
		if len(split) < 2 {
			log.Println("bad command")
			return
		}
		err := client.Delete(split[1])
		if err != nil {
			log.Println(err)
		}
	case "upload":
		if len(split) < 2 {
			log.Println("bad command")
			return
		}
		f, err := os.Open(split[1])
		if err != nil {
			log.Println(err)
			return
		}
		defer f.Close()
		err = client.Stor(split[1], f)
		if err != nil {
			log.Println(err)
		}
	case "download":
		if len(split) < 2 {
			log.Println("bad command")
			return
		}
		f, err := client.Retr(split[1])
		if err != nil {
			log.Println(err)
			return
		}
		defer f.Close()
		file, err := os.Create(split[1])
		if err != nil {
			log.Println(err)
			return
		}
		defer file.Close()
		_, err = io.Copy(file, f)
		if err != nil {
			log.Fatal(err)
		}
	case "/getNews":
		parseNews(client)
	default:
		log.Println("unknown command")
	}
}

func parseNews(client *ftp.ServerConn) {
	fp := gofeed.NewParser()
	log.Println("Making RSS parse...")
	feed, err := fp.ParseURL(URL_FOR_PARSE)
	if err != nil {
		log.Fatalf("fail with RSS parsing: %v", err)
	}
	oldNews := make([]string, 0)
	entries, err := client.List("/")
	if err != nil {
		log.Println(err)
		return
	}
	for _, entry := range entries {
		if strings.Split(entry.Name, "_")[0] == "Okutin" {
			f, err := client.Retr(entry.Name)
			if err != nil {
				log.Println(err)
				f.Close()
				return
			}
			scanner := bufio.NewScanner(f)
			c := 1
			for scanner.Scan() {
				if c%4 == 1 {
					oldNews = append(oldNews, scanner.Text())
				}
				c++
			}
			f.Close()
		}
	}
	curTime := time.Now()
	date := strings.Split(curTime.String(), " ")[0]
	timeForFile := strconv.Itoa(curTime.Hour()) + "-" + strconv.Itoa(curTime.Minute()) + "-" + strconv.Itoa(curTime.Second())
	filename := "Okutin_Denis_" + date + "_" + timeForFile + "_.txt"
	file, err := os.Create(filename)
	if err != nil {
		log.Println(err)
		return
	}
	for _, item := range feed.Items {
		f := 0
		for _, title := range oldNews {
			if item.Title == title {
				f = 1
				break
			}
		}
		if f == 0 {
			file.WriteString(item.Title + "\n" + item.Content + "\n" + item.Updated + "\n\n")
		}
	}
	file.Close()
	f, _ := os.Open(filename)
	err = client.Stor(filename, f)
	if err != nil {
		log.Println(err)
		return
	}
	log.Println("Successfully parsed")
}

func main() {
	client, err := ftp.Dial("students.yss.su:21")
	//client, err := ftp.Dial("localhost:2121")
	if err != nil {
		log.Fatal(err)
	}
	err = client.Login("ftpiu8", "3Ru7yOTA")
	//err = client.Login("admin", "123456")
	if err != nil {
		log.Fatal(err)
	}
	b := bufio.NewScanner(os.Stdin)
	for b.Scan() {
		cmd := b.Text()
		handleCommand(cmd, client)
	}
	if err = client.Quit(); err != nil {
		log.Fatal(err)
	}
}
