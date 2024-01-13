package config

import (
	"github.com/ilyakaznacheev/cleanenv"
)

type Config struct {
	Server struct {
		Host string `yaml:"host"`
		Port string `yaml:"port"`
	}
	IsDebug bool `yaml:"isDebug"`

	Database struct {
		Port     string `yaml:"port"`
		Host     string `yaml:"host"`
		User     string `yaml:"user"`
		Name     string `yaml:"name"`
		Password string `yaml:"password"`
		SSLMode  string `yaml:"ssl_mode"`
	}
}

func InitConfig() (*Config, error) {
	cfg := &Config{}
	err := cleanenv.ReadConfig("async/config/config.yml", cfg)
	return cfg, err
}
