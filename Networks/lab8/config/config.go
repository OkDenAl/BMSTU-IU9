package config

import (
	"github.com/ilyakaznacheev/cleanenv"
	"github.com/joho/godotenv"
)

type Config struct {
	FtpServer struct {
		Port     string `env:"FTP_SERVER_PORT" env-default:"2000"`
		User     string `env:"FTP_SERVER_USER" env-default:"admin"`
		Password string `env:"FTP_SERVER_PASS" env-default:"123456"`
	}
	HttpServer struct {
		Port string `end:"HTTP_SERVER_PORT" env-default:"8080"`
	}
}

func InitConfig() (*Config, error) {
	err := godotenv.Load()
	if err != nil {
		return nil, err
	}
	cfg := &Config{}
	err = cleanenv.ReadEnv(cfg)
	return cfg, err
}
