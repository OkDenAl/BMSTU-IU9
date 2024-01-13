package entity

import "github.com/golang-jwt/jwt/v4"

type Claims struct {
	Email  string
	UserId int
	jwt.RegisteredClaims
}

type Token struct {
	AccessToken  string `json:"accessToken"`
	RefreshToken string `json:"refreshToken"`
}
