package repository

import (
	"context"
	"database/sql"
	"lab9/async/entity"
)

type UserRepository interface {
	Create(ctx context.Context, user *entity.User) error
	GetUser(ctx context.Context, user entity.User) (*entity.User, error)
}

func NewUserRepository(db *sql.DB) repository {
	return repository{
		db: db,
	}
}

type repository struct {
	db *sql.DB
}

func (r repository) Create(ctx context.Context, user *entity.User) error {
	return r.db.QueryRow("INSERT INTO users (email,password) VALUES ($1,$2) RETURNING id", user.Email, user.Password).Scan(&user.UserId)
}
func (r repository) GetUser(ctx context.Context, user entity.User) (*entity.User, error) {
	row, err := r.db.Query("SELECT (id) FROM users WHERE email = $1", user.Email)
	if err != nil {
		return nil, err
	}
	if row.Next() {
		err = row.Scan(&user.UserId)
		if err != nil {
			return nil, err
		}
		return &user, nil
	} else {
		return nil, err
	}
}
