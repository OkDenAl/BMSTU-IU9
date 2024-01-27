USE master;
GO

IF DB_ID (N'lab6') IS NOT NULL
DROP DATABASE lab6;
GO

CREATE DATABASE lab6
ON
(
	NAME = lab6dat,
	FILENAME = 'C:\data\lab6dat.mdf', 
	SIZE = 10,
	MAXSIZE = UNLIMITED, 
	FILEGROWTH = 5%
)
LOG ON
(
	NAME = lab6_Log,
	FILENAME = 'C:\data\lab6log.ldf',
	SIZE = 5MB,
	MAXSIZE = 25MB,
	FILEGROWTH = 5MB
)
GO

--1.Создать таблицу с автоинкрементным первичным ключом.
--2.Добавить поля, для которых используются ограничения (CHECK), значения по умолчанию(DEFAULT), также использовать функции для вычисления.
USE lab6
GO
DROP TABLE IF EXISTS Client;
GO

-- @mail.ru
CREATE TABLE Client
(
	id INT IDENTITY(1,1) PRIMARY KEY,
    email NVARCHAR(80) UNIQUE NOT NULL CHECK (LEN(email) > 8),
    name NVARCHAR(50) NOT NULL,
    surname NVARCHAR(70) NOT NULL CHECK (LEN(surname) > 1) ,
	gender BIT NOT NULL,
    dateOfBirth Date NOT NULL CHECK (dateOfBirth < DATEADD(year, -12, GETDATE())) DEFAULT DATEADD(year, -12, GETDATE()),

	CONSTRAINT surname_check CHECK (LEN(name) > 1)
);
GO

INSERT INTO Client(email, name, surname, gender,dateOfBirth) VALUES
('email@mail.ru','Oleg','Petrov',1,'2002-02-06'),
('email1@mail.ru','Oleg1','Petrov1',1,'2000-02-06');
GO

SELECT @@IDENTITY as ClientID;
SELECT SCOPE_IDENTITY() as ClientID_scope;
SELECT IDENT_CURRENT('Client') as ClientID_current;

INSERT INTO Client(email, name, surname, gender,dateOfBirth) VALUES
('email2@mail.ru','Oleg','Petrov',1,'2002-02-06'),
('email3@mail.ru','Oleg1','Petrov1',1,'2000-02-06');
GO

SELECT @@IDENTITY as ClientID;
SELECT SCOPE_IDENTITY() as ClientID_scope;
SELECT IDENT_CURRENT('Client') as ClientID_current;

SELECT * FROM Client;
GO

--3.Создать таблицу с первичным ключом на основе глобального уникального идентификатора.
DROP TABLE IF EXISTS AbonementType;
GO

CREATE TABLE AbonementType
(
	id UNIQUEIDENTIFIER PRIMARY KEY DEFAULT(NEWID()),
	name NVARCHAR(40) UNIQUE NOT NULL CHECK (LEN(name) > 1),
	price FLOAT NOT NULL CHECK (price > 0),
	duration NVARCHAR(80) NOT NULL CHECK (duration IN('30 days', '1 year','90 days','7 days'))
);
GO

INSERT INTO AbonementType(name,price,duration) VALUES
('Base',100,'30 days'),
('Base+',200,'30 days');
GO

SELECT * FROM AbonementType;
GO

--4.Создать таблицу с первичным ключом на основе последовательности.
DROP SEQUENCE IF EXISTS FloorSeq;
GO

CREATE SEQUENCE CoachSeq 
    START WITH 1
    INCREMENT BY 1
    MAXVALUE 10;
GO

DROP TABLE IF EXISTS Coach;
GO

CREATE TABLE Coach
(
	id INT PRIMARY KEY,
    email NVARCHAR(80) UNIQUE NOT NULL,
    name NVARCHAR(50) NOT NULL,
    surname NVARCHAR(70) NOT NULL,
	gender BIT NOT NULL,
    dateOfBirth Date NOT NULL,
	salary INT NOT NULL,
	specialization NVARCHAR(120) DEFAULT 'coach'
);
GO

INSERT INTO Coach(id,email,name,surname,gender,dateOfBirth,salary) VALUES
(NEXT VALUE FOR CoachSeq, 'test@mail.ru','Ivan','Ivanov',1,'2000-02-06',100),
(NEXT VALUE FOR CoachSeq, 'test1@mail.ru','Ivan1','Ivanov',1,'2001-02-06',100),
(NEXT VALUE FOR CoachSeq, 'test2@mail.ru','Ivan2','Ivanov',1,'2002-02-06',100),
(NEXT VALUE FOR CoachSeq, 'test3@mail.ru','Ivan3','Ivanov',1,'2003-02-06',100),
(NEXT VALUE FOR CoachSeq, 'test4@mail.ru','Ivan4','Ivanov',1,'2004-02-06',100);
GO

SELECT * FROM Coach;
GO

-- 5.Создать две связанные таблицы, и протестировать на них различные варианты действий для
-- ограничений ссылочной целостности (NO ACTION |CASCADE | SET NULL | SET DEFAULT).

DROP TABLE IF EXISTS AbonementType;
CREATE TABLE AbonementType
(
	name NVARCHAR(40) PRIMARY KEY NOT NULL CHECK (LEN(name) > 1),
	price FLOAT NOT NULL CHECK (price > 0),
	duration NVARCHAR(80) NOT NULL CHECK (duration IN('30 days', '1 year','90 days','7 days'))
);
GO

DROP TABLE IF EXISTS Abonement;
CREATE TABLE Abonement
(
	id INT IDENTITY(1,1) PRIMARY KEY,
	date_ Date NOT NULL,
	expires_at Date NOT NULL CHECK (expires_at > DATEADD(year, -12, GETDATE())),
	abonement_type_name NVARCHAR(40) FOREIGN KEY REFERENCES AbonementType(name)
	--ON DELETE SET NULL
	--ON DELETE NO ACTION
	--ON DELETE SET DEFAULT
	ON DELETE CASCADE
);
GO

INSERT INTO AbonementType(name,price,duration) VALUES
('Base',100,'30 days'),
('Base+',200,'30 days');
SELECT * FROM AbonementType;
GO

INSERT INTO Abonement(date_, expires_at,abonement_type_name) VALUES
('2024-02-06','2024-02-06','Base'),
('2025-03-06','2025-02-06','Base+');
SELECT * FROM Abonement;
GO

--DELETE FROM AbonementType WHERE name='Base'

SELECT * FROM AbonementType;
SELECT * FROM Abonement;
GO

