FROM golang:1.23-alpine as builder

WORKDIR /app
COPY go.mod go.sum ./
RUN go mod download
COPY .. .

RUN CGO_ENABLED=0 GOOS=linux go build -o bigdatalab .

CMD ["./bigdatalab"]



