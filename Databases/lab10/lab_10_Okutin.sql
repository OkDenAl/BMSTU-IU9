/*Вообще лаба у меня в 3х файлах, отдельный файл для создания таблиц и 2 файла для транзакций (чтобы запускать их одновременно
и смотреть на их параллельную работу. Но для отправки скомпоновал всё в один. Всё закомментировано*/

/*Часть с инициализацией таблицы*/

/*/*USE master;
GO

IF DB_ID (N'lab10') IS NOT NULL
DROP DATABASE lab10;
GO

CREATE DATABASE lab10
ON
(
	NAME = lab10dat,
	FILENAME = 'C:\data\lab10dat.mdf', 
	SIZE = 10,
	MAXSIZE = UNLIMITED, 
	FILEGROWTH = 5%
)
LOG ON
(
	NAME = lab10_Log,
	FILENAME = 'C:\data\lab10log.ldf',
	SIZE = 5MB,
	MAXSIZE = 25MB,
	FILEGROWTH = 5MB
)
GO*/

USE lab10
GO
DROP TABLE IF EXISTS Client;
GO


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
('email1@mail.ru','Daria','Petrova',0,'2000-02-06'),
('email2@mail.ru','Yulia','Kazakova',0,'2003-02-06'),
('email3@mail.ru','Ivan','Petrov',1,'2001-02-06'),
('email4@mail.ru','Boris','Sidorov',1,'2005-02-06');
GO

SELECT * FROM Client; */


/*Часть с различными уровнями изоляции*/

/*
-- 1 Исследовать и проиллюстрировать на примерах
-- различные уровни изоляции транзакций MS SQL Server,
-- устанавливаемые с использованием
-- инструкции SET TRANSACTION ISOLATION LEVEL

USE lab10;
GO

-- 1) незавершенное чтение (read uncommited)

/*SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED;
BEGIN TRANSACTION;
	SELECT * FROM Client;
	WAITFOR DELAY '00:00:05';
	SELECT * FROM Client;
COMMIT TRANSACTION;
GO*/

-- 2) завершенное чтение (read commited)


/*SET TRANSACTION ISOLATION LEVEL READ COMMITTED;
BEGIN TRANSACTION;
    SELECT * FROM Client;
	WAITFOR DELAY '00:00:05';
	SELECT * FROM Client;
COMMIT TRANSACTION;
GO*/


-- 3) воспроизводимое чтение (repeatable read)

/*SET TRANSACTION ISOLATION LEVEL REPEATABLE READ;
BEGIN TRANSACTION;
    SELECT * FROM Client;
	wAITFOR DELAY '00:00:05';
	SELECT * FROM Client;
COMMIT TRANSACTION;
GO*/

-- 4) serializable

SET TRANSACTION ISOLATION LEVEL SERIALIZABLE
BEGIN TRANSACTION;
	SELECT * FROM Client;
	--WAITFOR DELAY '00:00:05';
	--SELECT * FROM Client;
COMMIT TRANSACTION;
GO
*/

-- Часть с тестовыми транзакциями (для проверки уровней изоляции)

/*
-- 2 Накладываемые блокировки исследовать с
-- использованием sys.dm_tran_locks

USE lab10;
GO

-- 1)

BEGIN TRANSACTION;
	UPDATE Client SET name = 'Vanya' WHERE name = 'Ivan';
	WAITFOR DELAY '00:00:05';
	ROLLBACK TRANSACTION
GO

-- 2)
/*BEGIN TRANSACTION;
	UPDATE Client SET name = 'Borya' WHERE name = 'Boris';
	-- WAITFOR DELAY '00:00:05';

	SELECT * FROM Client;
	--SELECT * FROM sys.dm_tran_locks;
    COMMIT TRANSACTION
GO*/

-- 3)
/*BEGIN TRANSACTION;
	UPDATE Client SET name = 'Name' WHERE name = 'Oleg';
    INSERT INTO Client(email, name, surname, gender,dateOfBirth) VALUES
    ('email7@mail.ru','Inna','Lumina',1,'2000-02-06');
	-- SELECT * FROM sys.dm_tran_locks;
    COMMIT TRANSACTION
GO*/

-- 4)

/*BEGIN TRANSACTION;
    INSERT INTO Client(email, name, surname, gender,dateOfBirth) VALUES
    ('email8@mail.ru','Alex','Star',1,'2003-02-06');
	-- SELECT * FROM sys.dm_tran_locks;
	SELECT * FROM Client;
    COMMIT TRANSACTION
GO*/
*/