package main

import (
	"bufio"
	"bytes"
	"fmt"
	"html/template"
	"log"
	"net/smtp"
	"os"
)

const INDEX_HTML = `
	<html>
		<body>
			<div>
				<table cellpadding="0" cellspacing="0" border="0" width="100%"
				style="background: whitesmoke; min-width: 320px; font-size: 1px; line-height: normal;">
					<tr>
						<td align="center" valign="top">
								<table cellpadding="0" cellspacing="0" border="5" width="700"
							style="background: black; color: whitesmoke; font-family: Arial, Helvetica, sans-serif;">
									<tr>
										<td align="center" valign="top">
											<span style="font-size: 20px; font-weight: bold; color: gold;
										line-height: 40px; -webkit-text-size-adjust:none; display: block;">
												Здравствуйте, Данила Павлович !
											</span>
											<hr width="600" size="1" color="whitesmoke" noshade>
											<span style="font-size: 16px; font-style: italic;
										line-height: 40px; -webkit-text-size-adjust:none; display: block;">
												{{.Msg}}
											</span>
										</td>
									</tr>
									<tr>
										<td align="center" valign="top">
											<img src="https://i1.wp.com/divedigital.id/wp-content/uploads/2021/12/48-1.jpg" alt="" width="350" height="350"/>
										</td>
									</tr>
									<tr>
									</tr>
								</table>
						</td>
					</tr>
				</table>
			</div>
		</body>
	</html>`

var indexHtml = template.Must(template.New("index").Parse(INDEX_HTML))

type Request struct {
	To   string
	Subj string
	Msg  string
}

func NewRequest(to, subj, msg string) *Request {
	return &Request{
		to,
		subj,
		msg,
	}
}

func (r *Request) sendMail(auth smtp.Auth) error {
	subject := fmt.Sprintf("Subject: %s\n", r.Subj)
	mime := "MIME-version: 1.0;\nContent-Type: text/html; charset=\"UTF-8\";\n\n"
	to := "To: " + r.To + "\n"
	msg := []byte(subject + to + mime + r.Msg)
	err := smtp.SendMail("smtp.gmail.com: 587", auth, "laba7denisspam@gmail.com",
		[]string{r.To}, msg)
	if err != nil {
		return err
	}
	return nil
}

func (r *Request) ParseTemplate(data interface{}) error {
	buf := new(bytes.Buffer)
	if err := indexHtml.Execute(buf, data); err != nil {
		return err
	}
	r.Msg = buf.String()
	return nil
}

func main() {
	var To, Subject, MessageBody string
	in := bufio.NewScanner(os.Stdin)
	fmt.Println("Введите email того, кому вы хотите отправить сообщение")
	in.Scan()
	To = in.Text()
	fmt.Println("Введите тему письма")
	in.Scan()
	Subject = in.Text()
	fmt.Println("Введите содержание письма")
	in.Scan()
	MessageBody = in.Text()
	auth := smtp.PlainAuth("", "laba7denisspam@gmail.com", "bykijovlsvgxfhau",
		"smtp.gmail.com")
	req := NewRequest(To, Subject, MessageBody)
	err := req.ParseTemplate(req)
	if err != nil {
		log.Fatal(err)
	}
	err = req.sendMail(auth)
	if err != nil {
		log.Fatal(err)
	}
}
