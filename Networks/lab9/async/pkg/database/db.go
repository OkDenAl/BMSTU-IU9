package database

import (
	"database/sql"
	"fmt"
	"github.com/golang-migrate/migrate/v4"
	"github.com/golang-migrate/migrate/v4/database/postgres"
	_ "github.com/golang-migrate/migrate/v4/source/file"
	_ "github.com/lib/pq"
	"lab9/async/config"
	"lab9/async/pkg/logger"
)

func New(cfg *config.Config, l logger.Logger) (*sql.DB, error) {
	DSN := fmt.Sprintf(
		"host=%s port=%s user=%s dbname=%s password=%s sslmode=%s",
		cfg.Database.Host, cfg.Database.Port, cfg.Database.User, cfg.Database.Name,
		cfg.Database.Password, cfg.Database.SSLMode)
	l.Debug("Connecting to DB:", DSN, "...")
	db, err := sql.Open("postgres", DSN)
	if err != nil {
		return nil, err
	}
	err = runMigrations(db, l)
	if err != nil {
		return nil, err
	}
	return db, nil
}

func runMigrations(db *sql.DB, l logger.Logger) error {
	driver, err := postgres.WithInstance(db, &postgres.Config{})
	if err != nil {
		return err
	}
	l.Debug("Executing migrations...")
	m, err := migrate.NewWithDatabaseInstance("file://async/pkg/database/migrations", "postgres", driver)
	if err != nil {
		return err
	}
	err = m.Up()
	if err != nil && err != migrate.ErrNoChange {
		return err
	}
	l.Debug("...done")
	return nil
}
