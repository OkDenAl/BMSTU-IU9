-- 1. Создать в базах данных пункта 1 задания 13 
-- таблицы, содержащие вертикально 
-- фрагментированные данные.

USE lab13db1
GO

DROP TABLE IF EXISTS Coach;
GO

CREATE TABLE Coach
(
	id INT PRIMARY KEY,
    email NVARCHAR(80) UNIQUE NOT NULL CHECK (LEN(email) > 8),
    name NVARCHAR(50) NOT NULL,
    surname NVARCHAR(70) NOT NULL CHECK (LEN(surname) > 1) ,

	CONSTRAINT coach_name_check CHECK (LEN(name) > 1)
);
GO

USE lab13db2
GO

DROP TABLE IF EXISTS Coach;
GO

CREATE TABLE Coach (
	id INT PRIMARY KEY,
	gender BIT NULL,
    dateOfBirth Date NULL CHECK (dateOfBirth < DATEADD(year, -18, GETDATE())),
	salary MONEY NOT NULL,
	specialization NVARCHAR(120) DEFAULT ('Coach'),
);
GO

-- 2.Создать необходимые элементы базы данных (представления, триггеры), обеспечивающие работу
-- с данными вертикально фрагментированных таблиц (выборку, вставку, изменение, удаление).

DROP VIEW IF EXISTS sectionCoachView
GO

CREATE VIEW sectionCoachView AS
	SELECT f.id, f.email, f.name, f.surname, s.gender, s.dateOfBirth, s.salary,s.specialization 
	FROM lab13db1.dbo.Coach f, lab13db2.dbo.Coach s
	WHERE f.id = s.id
GO

DROP TRIGGER IF EXISTS CoachViewInsert 
DROP TRIGGER IF EXISTS CoachViewUpdate 
DROP TRIGGER IF EXISTS CoachViewDelete 
GO

CREATE TRIGGER CoachViewInsert ON sectionCoachView
INSTEAD OF INSERT
AS
	INSERT INTO lab13db1.dbo.Coach(id, email, surname, name)
		SELECT inserted.id, inserted.email, inserted.surname, inserted.name FROM inserted
	INSERT INTO lab13db2.dbo.Coach(id, dateOfBirth,gender,salary,specialization)
		SELECT inserted.id, inserted.dateOfBirth, inserted.gender, inserted.salary, inserted.specialization FROM inserted
GO

INSERT INTO sectionCoachView(id,email, name, surname, gender,dateOfBirth,salary,specialization) VALUES
(5,'coach@mail.ru','Semen','Ivanov',1,'2002-02-06',500,'Coach'),
(1,'coach1@mail.ru','Daria','Vlasova',0,'2000-02-06',1000,'Coach');

SELECT * FROM sectionCoachView ORDER BY id DESC;
SELECT * FROM lab13db1.dbo.Coach;
SELECT * FROM lab13db2.dbo.Coach;
GO

CREATE TRIGGER CoachViewUpdate ON sectionCoachView
INSTEAD OF UPDATE
AS
	IF UPDATE(id)
		BEGIN
			RAISERROR('ERROR - YOU ARE NOT ALLOWED TO CHANGE Coach id', 14, -1, 'CoachViewUpd')
		END
	ELSE
		BEGIN
			UPDATE lab13db1.dbo.Coach
				SET email = inserted.email, surname = inserted.surname, name = inserted.name
					FROM inserted, lab13db1.dbo.Coach as f
					WHERE f.id= inserted.id
			UPDATE lab13db2.dbo.Coach
				SET dateOfBirth = inserted.dateOfBirth,	gender = inserted.gender, salary = inserted.salary, specialization= inserted.specialization
					FROM inserted, lab13db2.dbo.Coach as s
					WHERE s.id = inserted.id
		END
GO

UPDATE sectionCoachView SET specialization = 'mega super coach' WHERE id = 5;

SELECT * FROM lab13db1.dbo.Coach;
SELECT * FROM lab13db2.dbo.Coach;
GO

CREATE TRIGGER CoachViewDelete ON sectionCoachView
INSTEAD OF DELETE
AS
	DELETE f FROM lab13db1.dbo.Coach AS f
		INNER JOIN deleted ON f.id = deleted.id
	DELETE s FROM lab13db2.dbo.Coach AS s
		INNER JOIN deleted ON s.id = deleted.id
GO

DELETE FROM sectionCoachView WHERE salary < 900;

SELECT * FROM lab13db1.dbo.Coach;
SELECT * FROM lab13db2.dbo.Coach;
GO
