USE lab6;

--1. Создать представление на основе одной из таблиц задания 6.
DROP VIEW IF EXISTS AbonementView;
GO

CREATE VIEW AbonementView AS
	SELECT  a.id,a.date_, a.expires_at, a.abonement_type_name
	FROM Abonement as a
	WHERE abonement_type_name='Base+';
GO

/*INSERT INTO Abonement(date_, expires_at,abonement_type_name) VALUES
('2025-02-06','2024-02-06','Base+');
INSERT INTO AbonementView(date_, expires_at,abonement_type_name) VALUES
('2026-02-06','2025-02-06','Base+');*/


SELECT * FROM Abonement
SELECT * FROM AbonementView
GO


-- 2. Создать представление на основе полей обеих
-- связанных таблиц задания 6

DROP VIEW IF EXISTS AbonementView;
GO

CREATE VIEW AbonementView AS
	SELECT  a.id,a.date_, a.expires_at, a_t.name,a_t.price, a_t.duration
	FROM Abonement as a INNER JOIN AbonementType as a_t ON a.abonement_type_name=a_t.name
	WITH CHECK OPTION 
GO

/*INSERT INTO Abonement(date_, expires_at,abonement_type_name) VALUES
('2025-02-06','2024-02-06','Base+');
INSERT INTO AbonementView(date_, expires_at) VALUES
('2026-02-06','2025-02-06');*/

SELECt * FROM Abonement
SELECT * FROM AbonementType
SELECT * FROM AbonementView
GO

-- 3 Создание индекса для одной из таблиц задания 6, 
-- включив в него дополнительные неключевые поля

DROP INDEX IF EXISTS ClientNameIndex ON Client;
GO

CREATE INDEX ClientNameIndex
	ON Client (surname,name DESC)
	INCLUDE (email,dateOfBirth)
GO

SELECT surname,name, email, dateOfBirth FROM Client WHERE surname = 'Petrov' and name = 'Oleg';
GO

-- 4 Создание индексированного представления

DROP VIEW IF EXISTS ClientIndexView;
GO

CREATE VIEW ClientIndexView 
WITH SCHEMABINDING AS
	SELECT email,name, surname,gender,dateOfBirth
	FROM dbo.Client
	WHERE name = 'Oleg';
GO

DROP INDEX IF EXISTS ClientNameIndex ON ClientIndexView;
GO

CREATE UNIQUE CLUSTERED INDEX ClientNameIndex  
    ON ClientIndexView (email);
GO

Select * FROM ClientIndexView;