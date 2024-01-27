USE master;
GO

IF DB_ID (N'lab8') IS NOT NULL
DROP DATABASE lab8;
GO

CREATE DATABASE lab8
ON
(
	NAME = lab8dat,
	FILENAME = 'C:\data\lab8dat.mdf', 
	SIZE = 10,
	MAXSIZE = UNLIMITED, 
	FILEGROWTH = 5%
)
LOG ON
(
	NAME = lab8_Log,
	FILENAME = 'C:\data\lab8log.ldf',
	SIZE = 5MB,
	MAXSIZE = 25MB,
	FILEGROWTH = 5MB
)
GO

USE lab8
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

SELECT * FROM Client;

-- 1. Создать хранимую процедуру, производящую выборку
-- из некоторой таблицы и возвращающую результат
-- выборки в виде курсора

DROP PROCEDURE IF EXISTS get_client;
GO

CREATE PROCEDURE get_client 
	@cursor CURSOR VARYING OUTPUT 
AS
	SET @cursor = CURSOR FORWARD_ONLY STATIC FOR 
		SELECT id,email,name,surname
		FROM Client WHERE dateOfBirth <= '2003-02-06';
	OPEN @cursor;
GO

DECLARE @client_cursor CURSOR;
EXEC get_client @cursor = @client_cursor OUTPUT;

FETCH NEXT FROM @client_cursor;
WHILE (@@FETCH_STATUS = 0)
	BEGIN
		FETCH NEXT FROM @client_cursor;
	END

CLOSE @client_cursor ;
DEALLOCATE @client_cursor ;
GO

/*2. Модифицировать хранимую процедуру п.1. таким
образом, чтобы выборка осуществлялась с
формированием столбца, значение которого
формируется пользовательской функцией.*/

DROP FUNCTION IF EXISTS dbo.upper_name;
GO

CREATE FUNCTION dbo.upper_name (@name NVARCHAR(50))
RETURNS NVARCHAR(50) AS
BEGIN
    DECLARE @res NVARCHAR(50);
    SELECT @res = UPPER(@name);
RETURN @res
END;
GO

DROP PROCEDURE IF EXISTS upper_name_proc;
GO

CREATE PROCEDURE upper_name_proc
    @cursor CURSOR VARYING OUTPUT
AS
    SET @cursor = CURSOR FORWARD_ONLY STATIC FOR
		SELECT id, email, dbo.upper_name(name) AS name, surname
		FROM Client WHERE dateOfBirth>='2004-02-06';
    OPEN @cursor;
GO

DECLARE @client_cursor CURSOR;
EXEC upper_name_proc @cursor = @client_cursor OUTPUT;

FETCH NEXT FROM @client_cursor;
WHILE (@@FETCH_STATUS = 0)
	BEGIN
		FETCH NEXT FROM @client_cursor;
	END

CLOSE @client_cursor ;
DEALLOCATE @client_cursor ;
GO

-- 3 Создать хранимую процедуру, вызывающую процедуру
-- п.1., осуществляющую прокрутку возвращаемого
-- курсора и выводящую сообщения, сформированные из
-- записей при выполнении условия, заданного еще одной
-- пользовательской функцией.

DROP FUNCTION IF EXISTS dbo.is_petrov;
GO

CREATE FUNCTION dbo.is_petrov(@surname nvarchar(70))
RETURNS bit
AS
BEGIN
	IF @surname = 'Petrov'
		RETURN 1
	RETURN 0
END;
GO

DROP PROCEDURE IF EXISTS get_petrov;
GO

CREATE PROCEDURE get_petrov
AS
	DECLARE @this_client_cursor CURSOR
	EXEC get_client @cursor = @this_client_cursor OUTPUT

	DECLARE @id INT, @email VARCHAR(80), @name NVARCHAR(50), @surname NVARCHAR(70);
	FETCH NEXT FROM @this_client_cursor INTO @id, @email, @name, @surname;
	WHILE (@@FETCH_STATUS = 0)
		BEGIN
			IF dbo.is_petrov(@surname) = 1
				PRINT FORMATMESSAGE(N'Клиент по фамилии %s найден: Его имя = %s, Его email = %s', @surname, @name, @email);
			FETCH NEXT FROM @this_client_cursor INTO @id, @email, @name, @surname;
		END
	CLOSE @this_client_cursor
	DEALLOCATE @this_client_cursor
GO	

EXEC get_petrov
GO


-- 4. Модифицировать хранимую процедуру п.2. таким
-- образом, чтобы выборка формировалась с помощью
-- табличной функции.

DROP FUNCTION IF EXISTS dbo.client_table_func;
GO

CREATE FUNCTION dbo.client_table_func ()
RETURNS table AS
RETURN (
	SELECT id,email,dbo.upper_name(name) as name,surname
	FROM Client WHERE dateOfBirth<='2004-02-06'
)
GO

DROP PROCEDURE IF EXISTS upper_name_proc_with_table;
GO

CREATE PROCEDURE upper_name_proc_with_table
	@cursor CURSOR VARYING OUTPUT
AS
	SET @cursor = CURSOR 
		FORWARD_ONLY STATIC FOR
		SELECT * FROM dbo.client_table_func()
	OPEN @cursor;
GO

DECLARE @client_cursor CURSOR;
EXEC upper_name_proc_with_table @cursor = @client_cursor OUTPUT;

FETCH NEXT FROM @client_cursor;
WHILE (@@FETCH_STATUS = 0)
	BEGIN
		FETCH NEXT FROM @client_cursor;
	END

CLOSE @client_cursor;
DEALLOCATE @client_cursor;
GO