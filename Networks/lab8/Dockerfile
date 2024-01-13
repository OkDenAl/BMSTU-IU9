FROM golang:alpine
WORKDIR /lab8

COPY go.mod ./
COPY go.sum ./
RUN go mod download
RUN go mod verify

COPY . .
RUN go build -o server ./cmd/
CMD ["./server"]