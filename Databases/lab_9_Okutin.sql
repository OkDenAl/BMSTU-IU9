USE master;
GO

IF DB_ID('lab9') IS NOT NULL
DROP DATABASE lab9;
GO

CREATE DATABASE lab9
ON PRIMARY
(   NAME = lab9_dat,
    FILENAME = 'C:\data\lab9dat.ldf',
    SIZE = 10,
    MAXSIZE = 50,
    FILEGROWTH = 5 )
LOG ON
( NAME = lab9_log,
    FILENAME = 'C:\data\lab9log.ldf',
    SIZE = 5MB,
    MAXSIZE = 25MB,
    FILEGROWTH = 5MB );
GO

USE lab9;
GO

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
);
GO

DROP TABLE IF EXISTS History;
CREATE TABLE History 
(
    id INT IDENTITY PRIMARY KEY,
    abonement_id INT NOT NULL,
    operation NVARCHAR(200) NOT NULL,
    create_at DATETIME NOT NULL DEFAULT GETDATE(),
);

-- 1. ƒл€ одной из таблиц создать триггеры на вставку, удаление и обновление,
-- при выполнении заданных условий один из триггеров
-- должен инициировать возникновение ошибки (RAISERROR / THROW).

--	вставка

INSERT INTO AbonementType(name,price,duration) VALUES
('Base',100,'30 days'),
('Base+',200,'30 days'),
('NewBase',100,'30 days');
GO

DROP TRIGGER IF EXISTS Insert_Abonement
DROP TRIGGER IF EXISTS Delete_Abonement
DROP TRIGGER IF EXISTS Update_Abonement
GO

CREATE TRIGGER Insert_Abonement ON Abonement
	AFTER INSERT 
	AS
	INSERT INTO History (abonement_id, operation)
	SELECT id, 'ƒобавлен абонемент c типом ' + abonement_type_name
	FROM inserted
GO


INSERT INTO Abonement(date_,expires_at,abonement_type_name) VALUES
(CONVERT(date, GETDATE()), CONVERT(date, N'02-01-2024'),'Base'),
(CONVERT(date, GETDATE()), CONVERT(date, N'03-01-2024'),'Base'),
(CONVERT(date, GETDATE()), CONVERT(date, N'02-01-2024'),'Base+'),
(CONVERT(date, GETDATE()), CONVERT(date, N'03-01-2024'),'Base+'),
(CONVERT(date, GETDATE()), CONVERT(date, N'02-01-2024'),'NewBase'),
(CONVERT(date, GETDATE()), CONVERT(date, N'03-01-2024'),'NewBase');


SELECT * FROM AbonementType;
SELECT * FROM Abonement;
SELECT * FROM History;
GO


--	удаление
CREATE TRIGGER Delete_Abonement ON Abonement
	AFTER DELETE 
	AS
	INSERT INTO History (abonement_id, operation)
	SELECT id, '”дален абонемент c типом ' + abonement_type_name
	FROM deleted
GO  

DELETE FROM Abonement WHERE abonement_type_name='Base';
GO

SELECT * FROM Abonement;
SELECT * FROM History;
GO

-- обновление
CREATE TRIGGER Update_Abonement ON Abonement        
	AFTER UPDATE 
	AS
	INSERT INTO History (abonement_id, operation)
	SELECT id, 'ќбновлен абонемент c типом ' + abonement_type_name
	FROM inserted
GO


UPDATE Abonement SET expires_at = GETDATE() WHERE abonement_type_name ='NewBase';
GO

SELECT * FROM Abonement;
SELECT * FROM History;
GO
GO



-- 2. ƒл€ представлени€ создать триггеры на вставку, удаление и добавление,
-- обеспечивающие возможность выполнени€ операций с данными 
-- непосредственно через представление.

DROP VIEW IF EXISTS AbonementView;
GO

CREATE VIEW AbonementView AS
	SELECT  a.id, a.date_, a.expires_at, a_t.name, a_t.price, a_t.duration
	FROM Abonement as a INNER JOIN AbonementType as a_t ON a.abonement_type_name=a_t.name
GO

-- error
--INSERT INTO AbonementView(date_, expires_at, name, price, duration) VALUES ('2023-02-06','2023-03-06','NewBase',400,'30 days');
--GO

DROP TRIGGER IF EXISTS Insert_AbonementView
DROP TRIGGER IF EXISTS Delete_AbonementView
DROP TRIGGER IF EXISTS Update_AbonementView
GO

DROP TRIGGER IF EXISTS Insert_Abonement
DROP TRIGGER IF EXISTS Delete_Abonement
DROP TRIGGER IF EXISTS Update_Abonement
GO

-- вставка
CREATE TRIGGER Insert_AbonementView ON AbonementView
	INSTEAD OF INSERT 
	AS
		BEGIN
		DECLARE @id int, @date_ Date, @expires_at Date, @name NVARCHAR(40), @price float, @duration NVARCHAR(80);
		
		DECLARE @cursor CURSOR;
		SET @cursor = CURSOR FOR SELECT * FROM inserted;
		OPEN @cursor;

		FETCH NEXT FROM @cursor INTO @id,@date_,@expires_at,@name,@price,@duration;
		WHILE (@@FETCH_STATUS = 0)
		BEGIN
		IF @name IN (SELECT name FROM AbonementType)
				IF @price = (SELECT price FROM AbonementType WHERE name = @name) AND @duration = (SELECT duration FROM AbonementType WHERE name = @name)
					INSERT INTO Abonement(date_,expires_at,abonement_type_name) VALUES (@date_,@expires_at,@name)
				ELSE
					THROW 51000, 'failed to insert data: cant find the abonement type data', 1;
		ELSE
			BEGIN
				INSERT INTO AbonementType(name,price,duration) VALUES (@name, @price,@duration)
				INSERT INTO Abonement(date_,expires_at,abonement_type_name) VALUES (@date_,@expires_at,@name)
			END
		FETCH NEXT FROM @cursor INTO @id,@date_,@expires_at,@name,@price,@duration;
		END
		CLOSE @cursor;
		DEALLOCATE @cursor;
		END
GO

SELECT * FROM AbonementView;
SELECT * FROM Abonement;
GO
-- error
INSERT INTO AbonementView(date_, expires_at, name, price, duration) VALUES 
--('2023-02-06','2023-03-06','Base',100,'30 days'),
('2023-07-06','2023-07-22','Base++',222,'7 days'),
('2023-02-06','2023-05-06','Base+',20,'30 days');
GO

SELECT * FROM Abonement;
SELECT * FROM AbonementView;
SELECt * FROm AbonementType;
GO

-- удаление
CREATE TRIGGER Delete_AbonementView ON AbonementView
	INSTEAD OF DELETE 
	AS
	BEGIN
		DELETE FROM Abonement WHERE id IN(SELECT id FROM deleted)
	END
GO

SELECT * FROM AbonementView;
-- DELETE FROM AbonementView WHERE name='Base+'
SELECT * FROM AbonementView;
GO

-- обновление
CREATE TRIGGER Update_AbonementView ON AbonementView
	INSTEAD OF UPDATE 
	AS 
	BEGIN
		IF UPDATE(duration) OR UPDATE(price)
			THROW 51000, 'failed to update data: cant update duration or price', 1;

		IF UPDATE(name)
			IF EXISTS(SELECT 1 FROM inserted INNER JOIN AbonementType on AbonementType.name = inserted.name)
				UPDATE Abonement SET Abonement.expires_at = inserted.expires_at, Abonement.date_ = inserted.date_ ,
				Abonement.abonement_type_name = inserted.name
				FROM Abonement INNER JOIN  inserted ON Abonement.id = inserted.id
			ELSE
				THROW 51000, 'failed to update data: abonement name does not exists', 1;
		ELSE
			BEGIN
				UPDATE Abonement SET Abonement.expires_at = inserted.expires_at, Abonement.date_ = inserted.date_
				FROM Abonement INNER JOIN  inserted ON Abonement.id = inserted.id
			END
	END
GO


SELECT * FROM AbonementView;
GO
-- error
UPDATE AbonementView SET name='Base+' WHERE id = 5 
--UPDATE AbonementView SET price=850 WHERE name = 'NewBase'
--UPDATE AbonementView SET date_= '2025-05-06' WHERE name = 'Base'
GO

SELECT * FROM AbonementView;
SELECT * FROM Abonement;
GO