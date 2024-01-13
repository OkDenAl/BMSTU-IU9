package utils

import (
	"errors"
	"github.com/golang-jwt/jwt/v4"
	"lab9/async/entity"
)

// ValidateAccessToken checks is the access token valid.
func ValidateAccessToken(accessToken string, id int) bool {
	claims := &entity.Claims{}
	token, err := jwt.ParseWithClaims(accessToken, claims, func(token *jwt.Token) (interface{}, error) {
		_, ok := token.Method.(*jwt.SigningMethodHMAC)
		if !ok {
			return nil, errors.New("invalid access token")
		}
		return []byte("access"), nil
	})
	if err != nil {
		return false
	}
	return token != nil && token.Claims.(*entity.Claims).UserId == id
}

// ValidateRefreshToken checks is the refresh token valid.
func ValidateRefreshToken(refreshToken string) bool {
	claims := &entity.Claims{}
	token, err := jwt.ParseWithClaims(refreshToken, claims, func(token *jwt.Token) (interface{}, error) {
		_, ok := token.Method.(*jwt.SigningMethodHMAC)
		if !ok {
			return nil, errors.New("invalid token")
		}
		return []byte("refresh"), nil
	})
	if err != nil {
		return false
	}
	return token != nil
}
