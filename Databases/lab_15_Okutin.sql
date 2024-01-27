 -- 1. Создать в базах данных пункта 1 задания 13 связанные таблицы.
USE lab13db1
GO

DROP TABLE IF EXISTS AbonementType;
GO

DROP TABLE IF EXISTS AbonementType;
CREATE TABLE AbonementType
(
	name NVARCHAR(40) PRIMARY KEY NOT NULL CHECK (LEN(name) > 1),
	price FLOAT NOT NULL CHECK (price > 0),
	duration NVARCHAR(80) NOT NULL CHECK (duration IN('30 days', '1 year','90 days','7 days'))
);
GO

USE lab13db2
GO

DROP TABLE IF EXISTS Abonement;
CREATE TABLE Abonement
(
	id INT IDENTITY(1,1) PRIMARY KEY,
	date_ Date NOT NULL,
	expires_at Date NOT NULL CHECK (expires_at > DATEADD(year, -12, GETDATE())),
	abonement_type_name NVARCHAR(40)
);
GO

-- 2. Создать необходимые элементы базы данных (представления, триггеры), обеспечивающие работу
-- с данными связанных таблиц (выборку, вставку, изменение, удаление).

DROP VIEW IF EXISTS AbonementView
GO
CREATE VIEW AbonementView AS
	SELECT  a.id,a.date_, a.expires_at, a_t.name,a_t.price, a_t.duration
	FROM Abonement as a INNER JOIN lab13db1.dbo.AbonementType as a_t ON a.abonement_type_name=a_t.name
GO

-- вставка без особенностей
DROP TRIGGER IF EXISTS AbonementTypeUpdate;
DROP TRIGGER IF EXISTS AbonementTypeDelete;
GO

USE lab13db1
GO

CREATE TRIGGER AbonementTypeUpdate ON AbonementType
FOR UPDATE
AS
	IF UPDATE(name)
		BEGIN
			RAISERROR('ERROR - YOU ARE NOT ALLOWED TO CHANGE AbonementType name', 16, 1, 'AbonementTypeUpdate')
			ROLLBACK
		END
GO

CREATE TRIGGER AbonementTypeDelete ON AbonementType
FOR DELETE
AS
	DELETE table2 FROM lab13db2.dbo.Abonement AS table2
		INNER JOIN deleted ON table2.abonement_type_name = deleted.name
GO

USE lab13db2
GO

-- удаление без особенностей
DROP TRIGGER IF EXISTS AbonementInsert -- вставка только, если есть тип абонемента
DROP TRIGGER IF EXISTS AbonementUpdate --  нельзя менять ID  и изменение только если есть тип абонемента
GO

CREATE TRIGGER AbonementInsert ON Abonement
FOR INSERT
AS
	IF EXISTS (SELECT * FROM lab13db1.dbo.AbonementType, inserted  
				WHERE lab13db1.dbo.AbonementType.name <> inserted.abonement_type_name)
		BEGIN
			RAISERROR('ERROR - AbonementType DOES NOT EXIST. YOU ARE NOT ALLOWED TO ADD Abonements', 16, 1, 'AbonementInsert')
			ROLLBACK
		END
GO

CREATE TRIGGER AbonementUpdate ON Abonement
FOR UPDATE
AS
	IF UPDATE(abonement_type_name) AND EXISTS (SELECT 1 FROM lab13db1.dbo.AbonementType RIGHT JOIN inserted 
				ON lab13db1.dbo.AbonementType.name = inserted.abonement_type_name
				WHERE lab13db1.dbo.AbonementType.name IS NULL)
		BEGIN
			RAISERROR('ERROR - Abonement Type DOES NOT EXIST. YOU ARE NOT ALLOWED TO ADD Abonements', 16, 2, 'AbonementUpdate')
			ROLLBACK
		END
GO

INSERT INTO lab13db1.dbo.AbonementType(name,price,duration) VALUES
('Base',100,'30 days'),
('Base+',200,'30 days');
GO

INSERT INTO Abonement(date_,expires_at,abonement_type_name) VALUES
(GETDATE(),DATEADD(day,30,GETDATE()),'Base'),
(GETDATE(),DATEADD(day,30,GETDATE()),'Base+'),
(GETDATE(),DATEADD(day,30,GETDATE()),'Baz');
GO

--INSERT INTO Abonement(date_,expires_at,abonement_type_name)
--VALUES (GETDATE(),DATEADD(day,30,GETDATE()),'Premium')
--GO

SELECT * FROM lab13db1.dbo.AbonementType
SELECT * FROM lab13db2.dbo.Abonement
SELECT * FROM AbonementView
GO

--UPDATE lab13db1.dbo.AbonementType SET name='Premium' WHERE name='Base+'

UPDATE lab13db1.dbo.AbonementType SET price=1000 WHERE name='Base+'

--UPDATE Abonement SET abonement_type_name='Premium' WHERE id=1

UPDATE Abonement SET abonement_type_name='Base+' WHERE id=1

SELECT * FROM lab13db1.dbo.AbonementType
SELECT * FROM lab13db2.dbo.Abonement
SELECT * FROM AbonementView
GO

DELETE FROM lab13db1.dbo.AbonementType WHERE name='Base+'

SELECT * FROM lab13db1.dbo.AbonementType
SELECT * FROM lab13db2.dbo.Abonement
SELECT * FROM AbonementView
GO