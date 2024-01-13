package mailer

import (
	"bytes"
	"context"
	"fmt"
	"lab7/spamer/entity"
	"lab7/spamer/pkg/logger"
	"lab7/spamer/repository/mailer"
	"math/rand"
	"net/smtp"
	"sync"
	"time"
)

type MailService interface {
	SendEmail(ctx context.Context) error
}

func NewMailService(repo mailer.Repository) *service {
	return &service{repo}
}

type service struct {
	db mailer.Repository
}

func (s *service) SendEmail(ctx context.Context, l logger.Logger) error {
	return s.db.Transaction(ctx, func(Txctx context.Context) error {
		users, err := s.db.GetAllUsers(Txctx)
		if err != nil {
			return err
		}
		var wg sync.WaitGroup
		for _, user := range users {
			err = parseTemplate(&user)
			if err != nil {
				return err
			}
			wg.Add(1)
			go func() {
				err = sendMail(user, &wg, l)
			}()
			wg.Wait()
			/*time.Sleep(time.Second)*/
			if err != nil {
				return err
			}
		}
		return nil
	})
}

func sendMail(u entity.User, wg *sync.WaitGroup, l logger.Logger) error {
	rand.Seed(1234)
	defer wg.Done()
	l.Debug(u)
	auth := smtp.PlainAuth("", "laba7denisspam@gmail.com", "bykijovlsvgxfhau",
		"smtp.gmail.com")
	subject := fmt.Sprintf("Subject: Лаба 11 тесты\n")
	mime := "MIME-version: 1.0;\nContent-Type: text/html; charset=\"UTF-8\";\n\n"
	to := "To: " + u.Email + "\n"
	msg := []byte(subject + to + mime + u.Message)
	err := smtp.SendMail("smtp.gmail.com: 587", auth, "laba7denisspam@gmail.com",
		[]string{u.Email}, msg)
	if err != nil {
		return err
	}
	dur := time.Duration(1000 * 1000 * 1000 * (rand.Intn(10)))
	l.Debug(dur)
	time.Sleep(dur)
	return nil
}

func parseTemplate(u *entity.User) error {
	buf := new(bytes.Buffer)
	if err := entity.IndexHtml.Execute(buf, u); err != nil {
		return err
	}
	u.Message = buf.String()
	return nil
}
