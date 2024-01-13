package database

import (
	"context"
	"database/sql"
	"fmt"
	_ "github.com/go-sql-driver/mysql"
	"lab7/spamer/config"
)

type mysql struct {
	db *sql.DB
}

func InitDb(cfg *config.Config) (*mysql, error) {
	params := fmt.Sprintf("%s:%s@tcp(%s)/%s", cfg.DB_User, cfg.DB_Password, cfg.DB_Host, cfg.DB_Name)
	db, err := sql.Open("mysql", params)
	if err != nil {
		return nil, err
	}
	return &mysql{db}, nil
}

func (d *mysql) Close() error {
	return d.db.Close()
}

func (d *mysql) Exec(ctx context.Context, query string, args ...interface{}) error {
	if tx := extractTx(ctx); tx != nil {
		_, err := tx.ExecContext(ctx, query, args...)
		if err != nil {
			return err
		}
	}
	_, err := d.db.ExecContext(ctx, query, args...)
	return err
}

func (d *mysql) Query(ctx context.Context, query string, args ...interface{}) (*sql.Rows, error) {
	if tx := extractTx(ctx); tx != nil {
		_, err := tx.QueryContext(ctx, query, args...)
		if err != nil {
			return nil, err
		}
	}
	rows, err := d.db.QueryContext(ctx, query, args...)
	return rows, err
}

func (d *mysql) QueryRow(ctx context.Context, query string, args ...interface{}) *sql.Row {
	if tx := extractTx(ctx); tx != nil {
		return tx.QueryRowContext(ctx, query, args...)
	}
	return d.db.QueryRowContext(ctx, query, args...)
}

type ctxKey string

const txKey = ctxKey("transaction")

func (d *mysql) WithTransaction(ctx context.Context, level sql.IsolationLevel, f func(ctx context.Context) error) error {
	tx, err := d.db.BeginTx(ctx, &sql.TxOptions{Isolation: level})
	if err != nil {
		return err
	}
	err = f(context.WithValue(ctx, txKey, level))
	if err != nil {
		_ = tx.Rollback()
		return err
	}
	return tx.Commit()
}

func extractTx(ctx context.Context) *sql.Tx {
	if tx, ok := ctx.Value(txKey).(*sql.Tx); ok {
		return tx
	}
	return nil
}
