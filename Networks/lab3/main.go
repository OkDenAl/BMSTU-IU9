package main

import (
	"database/sql"
	"fmt"
	_ "github.com/go-sql-driver/mysql"
	"github.com/mmcdole/gofeed"
	"log"
	"os"
	"strings"
)

const (
	HOST          = "students.yss.su"
	USER          = "iu9networkslabs"
	PASSWORD      = "Je2dTYr6"
	DBNAME        = "iu9networkslabs"
	URL_FOR_PARSE = "https://news.google.com/atom?topic=t&hl=ru&gl=RU&ceid=RU:ru"
)

func splitDateAndTime(updated string) (date, time string) {
	splitArr := strings.Split(updated, "T")
	return splitArr[0], strings.Split(splitArr[1], ".")[0]
}

func personToStr(p gofeed.Person) string {
	return p.Name + p.Email
}

func cleanDB(db *sql.DB) error {
	_, err := db.Exec("TRUNCATE TABLE iu9okutin")
	return err
}

func main() {
	logger := log.New(os.Stdout, "", 1)
	params := fmt.Sprintf("%s:%s@tcp(%s)/%s", USER, PASSWORD, HOST, DBNAME)
	logger.Println("Connecting to db with params:\n" + params)
	db, err := sql.Open("mysql", params)
	if err != nil {
		logger.Fatalf("fail with db connection: %v", err)
	}
	defer db.Close()
	fp := gofeed.NewParser()
	logger.Println("Making RSS parse...")
	feed, err := fp.ParseURL(URL_FOR_PARSE)
	if err != nil {
		logger.Fatalf("fail with RSS parsing: %v", err)
	}
	logger.Println("Successfully parsed")
	for _, item := range feed.Items {
		row, err := db.Query("SELECT EXISTS(SELECT title FROM iu9okutin WHERE title=?)", item.Title)
		if err != nil {
			logger.Fatal(err)
		}
		var f bool
		row.Next()
		row.Scan(&f)
		if !f {
			date, time := splitDateAndTime(item.Updated)
			_, err = db.Exec("INSERT INTO iu9okutin (title,text,date,time,author) VALUES "+
				"(?,?,?,?,?)", item.Title, item.Content, date, time, personToStr(*feed.Authors[0]))
			if err != nil {
				logger.Fatal(err)
			}
		}
	}
}
