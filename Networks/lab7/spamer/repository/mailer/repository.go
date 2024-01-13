package mailer

import (
	"context"
	"database/sql"
	"lab7/spamer/entity"
	"lab7/spamer/pkg/database"
)

type Repository interface {
	GetAllUsers(ctx context.Context) ([]entity.User, error)
	Transaction(ctx context.Context, f func(ctx context.Context) error) error
}

type repo struct {
	db database.Database
}

func New(db database.Database) *repo {
	return &repo{db}
}
func (r *repo) GetAllUsers(ctx context.Context) ([]entity.User, error) {
	query := "SELECT Name,Email,Message FROM iu9okutinSMTP"
	rows, err := r.db.Query(ctx, query)
	if err != nil {
		return nil, err
	}
	users := make([]entity.User, 0)
	user := entity.User{}
	for rows.Next() {
		err = rows.Scan(&user.Name, &user.Email, &user.Message)
		if err != nil {
			return nil, err
		}
		users = append(users, user)
	}
	return users, nil
}

func (r *repo) Transaction(ctx context.Context, f func(ctx context.Context) error) error {
	return r.db.WithTransaction(ctx, sql.LevelReadCommitted, f)
}
