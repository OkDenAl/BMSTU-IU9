package config

import "github.com/ilyakaznacheev/cleanenv"

type Config struct {
	DB_User     string `yaml:"db_user"`
	DB_Password string `yaml:"db_password"`
	DB_Host     string `yaml:"db_host"`
	DB_Name     string `yaml:"db_name"`
	IsDebug     bool   `yaml:"is_debug"`
}

func InitConfig() (*Config, error) {
	cfg := Config{}
	err := cleanenv.ReadConfig("spamer/config/config.yml", &cfg)
	if err != nil {
		return nil, err
	}
	return &cfg, nil
}
