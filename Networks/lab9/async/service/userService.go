package service

import (
	"context"
	"errors"
	"fmt"
	"github.com/asaskevich/govalidator"
	jwt "github.com/golang-jwt/jwt/v4"
	"lab9/async/entity"
	"lab9/async/repository"
	"time"
)

type UserService interface {
	Registration(ctx context.Context, user entity.User) (*entity.UserData, error)
	Login(ctx context.Context, user entity.User) (*entity.UserData, error)
}

func NewUserService(repo repository.UserRepository) *service {
	return &service{repo: repo}
}

type service struct {
	repo repository.UserRepository
}

func (s *service) Login(ctx context.Context, user entity.User) (*entity.UserData, error) {
	if !govalidator.IsEmail(user.Email) {
		return nil, fmt.Errorf("error with password")
	}
	u, err := s.repo.GetUser(ctx, user)
	if err != nil {
		return nil, err
	}
	if u == nil {
		return nil, errors.New("пользователь с таким email не существует")
	}
	if u.Password != user.Password {
		return nil, errors.New("invalid password")
	}
	tokens, err := GenerateTokens(ctx, u.Email, u.UserId)
	if err != nil {
		return nil, err
	}
	return &entity.UserData{
		*u,
		tokens,
	}, nil
}

func (s *service) Registration(ctx context.Context, user entity.User) (*entity.UserData, error) {
	if !govalidator.IsEmail(user.Email) || !govalidator.ByteLength(user.Password, "4", "16") {
		return nil, fmt.Errorf("error with password")
	}
	u, err := s.repo.GetUser(ctx, user)
	if err != nil {
		return nil, err
	}
	if u != nil {
		return nil, errors.New("пользователь с таким email уже существует")
	}
	err = s.repo.Create(ctx, &user)
	if err != nil {
		return nil, err
	}
	tokens, err := GenerateTokens(ctx, user.Email, user.UserId)
	if err != nil {
		return nil, err
	}
	return &entity.UserData{
		user,
		tokens,
	}, nil
}

func GenerateTokens(ctx context.Context, email string, userId int) (entity.Token, error) {
	claims := &entity.Claims{
		UserId: userId,
		Email:  email,
		RegisteredClaims: jwt.RegisteredClaims{
			ExpiresAt: &jwt.NumericDate{Time: time.Now().Add(5 * time.Minute)},
		},
	}
	accessToken := jwt.NewWithClaims(jwt.SigningMethodHS256, claims)
	accessTokenString, err := accessToken.SignedString([]byte("access"))
	if err != nil {
		return entity.Token{}, err
	}
	claims.ExpiresAt = &jwt.NumericDate{Time: time.Now().Add(30 * 24 * time.Hour)}
	refreshToken := jwt.NewWithClaims(jwt.SigningMethodHS256, claims)
	refreshTokenString, err := refreshToken.SignedString([]byte("refresh"))
	if err != nil {
		return entity.Token{}, err
	}
	return entity.Token{AccessToken: accessTokenString, RefreshToken: refreshTokenString}, nil
}
