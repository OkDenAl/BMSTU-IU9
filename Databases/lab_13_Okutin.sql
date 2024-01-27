-- Создать две базы данных на одном экземпляре СУБД SQL Server

USE master;
GO

IF DB_ID('lab13db1') IS NOT NULL
DROP DATABASE lab13db1;
GO

IF DB_ID('lab13db2') IS NOT NULL
DROP DATABASE lab13db2;
GO

CREATE DATABASE lab13db1
ON 
( NAME = lab13db1_dat,
    FILENAME = 'C:\data\lab13db1dat.mdf',
    SIZE = 10,
    MAXSIZE = 50,
    FILEGROWTH = 5 )
LOG ON
( NAME = lab13db1_log,
    FILENAME = 'C:\data\lab13db1log.mdf',
    SIZE = 5MB,
    MAXSIZE = 25MB,
    FILEGROWTH = 5MB );
GO 

CREATE DATABASE lab13db2
ON 
( NAME = lab13db2_dat,
    FILENAME = 'C:\data\lab13db2dat.mdf',
    SIZE = 10,
    MAXSIZE = 50,
    FILEGROWTH = 5 )
LOG ON
( NAME = lab13db2_log,
    FILENAME = 'C:\data\lab13db2log.mdf',
    SIZE = 5MB,
    MAXSIZE = 25MB,
    FILEGROWTH = 5MB );
GO

-- Создать в базах данных п.1. горизонтально фрагментированные таблицы.
USE lab13db1
GO

DROP TABLE IF EXISTS Client;
GO

CREATE TABLE Client
(
	id INT PRIMARY KEY CHECK (id <= 2),
    email NVARCHAR(80) UNIQUE NOT NULL CHECK (LEN(email) > 8),
    name NVARCHAR(50) NOT NULL,
    surname NVARCHAR(70) NOT NULL CHECK (LEN(surname) > 1) ,
	gender BIT NULL,
    dateOfBirth Date NULL CHECK (dateOfBirth < DATEADD(year, -14, GETDATE())),

	CONSTRAINT client_name_check CHECK (LEN(name) > 1)
);
GO


USE lab13db2
GO

DROP TABLE IF EXISTS Client;
GO

CREATE TABLE Client
(
	id INT NOT NULL PRIMARY KEY CHECK (id > 2),
    email NVARCHAR(80) UNIQUE NOT NULL CHECK (LEN(email) > 8),
    name NVARCHAR(50) NOT NULL,
    surname NVARCHAR(70) NOT NULL CHECK (LEN(surname) > 1) ,
	gender BIT NULL,
    dateOfBirth Date NULL CHECK (dateOfBirth < DATEADD(year, -14, GETDATE())),

	CONSTRAINT client_name_check CHECK (LEN(name) > 1)
);

--Создать секционированные представления, обеспечивающие работу с данными таблиц
-- (выборку, вставку, изменение, удаление).
DROP VIEW IF EXISTS sectionClientView
GO

CREATE VIEW sectionClientView AS
	SELECT * FROM lab13db1.dbo.Client
	UNION ALL
	SELECT * FROM lab13db2.dbo.Client
GO


INSERT INTO sectionClientView(id,email, name, surname, gender,dateOfBirth) VALUES
(1,'oleg@mail.ru','Oleg','Petrov',1,'2002-02-06'),
(10,'dasha@mail.ru','Daria','Razumova',0,'2000-02-06'),
(5,'nikita@mail.ru','Nikita','Lavrentiev',1,'2000-06-06'),
(2,'misha@mail.ru','Mihail','Rastorguev',1,'1999-09-09');
GO


SELECT * FROM sectionClientView ORDER BY id;

SELECT * FROM lab13db1.dbo.Client
SELECT * FROM lab13db2.dbo.Client
GO

UPDATE sectionClientView
SET name = 'Olya' WHERE id = 10
GO

SELECT * FROM lab13db1.dbo.Client
SELECT * FROM lab13db2.dbo.Client
GO

DELETE FROM sectionClientView
WHERE gender=0
GO 

SELECT * FROM lab13db1.dbo.Client
SELECT * FROM lab13db2.dbo.Client
GO