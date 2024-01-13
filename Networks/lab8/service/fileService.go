package service

import (
	"bytes"
	"fmt"
	"github.com/jlaffaye/ftp"
	"io"
	"lab8/api/protocol"
	apperrors "lab8/pkg/error"
	"os"
	"os/exec"
	"path/filepath"
)

type FileService struct {
	client *ftp.ServerConn
}

func NewFileService(client *ftp.ServerConn) *FileService {
	return &FileService{
		client: client,
	}
}

func (s FileService) ServeFile(path string, args []string) (*bytes.Buffer, *protocol.Status, error) {
	f, err := s.client.Retr(path)
	if err != nil {
		return nil, nil, apperrors.BadRequest
	}
	defer f.Close()

	if filepath.Ext(path) != ".cpp" {
		var out bytes.Buffer
		_, err = io.Copy(&out, f)
		return &out, &protocol.Status{IsCpp: false}, err
	}
	_, codeFileName := filepath.Split(path)
	fmt.Println(codeFileName)
	objectFileName := codeFileName[:len(codeFileName)-3] + "exe"
	file, err := os.Create(codeFileName)
	if err != nil {
		return nil, nil, err
	}
	_, err = io.Copy(file, f)
	if err != nil {
		return nil, nil, err
	}
	defer os.Remove(file.Name())
	file.Close()

	cmd := exec.Command("g++", codeFileName, "-o", objectFileName)
	if err = cmd.Run(); err != nil {
		return nil, nil, err
	}
	cmd = exec.Command("./"+objectFileName, args...)
	var out bytes.Buffer
	cmd.Stdout = &out
	if err = cmd.Run(); err != nil {
		return nil, nil, err
	}
	os.Remove(objectFileName)
	return &out, &protocol.Status{IsCpp: true}, nil
}
