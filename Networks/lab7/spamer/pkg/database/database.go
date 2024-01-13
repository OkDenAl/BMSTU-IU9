package database

import (
	"context"
	"database/sql"
)

type Database interface {
	Exec(ctx context.Context, query string, args ...interface{}) error
	Query(ctx context.Context, query string, args ...interface{}) (*sql.Rows, error)
	QueryRow(ctx context.Context, query string, args ...interface{}) *sql.Row
	WithTransaction(ctx context.Context, level sql.IsolationLevel, f func(ctx context.Context) error) error
	Close() error
}
