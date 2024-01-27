USE master;
GO

IF DB_ID (N'lab11') IS NOT NULL
DROP DATABASE lab11;
GO

CREATE DATABASE lab11
ON
(
	NAME = lab11dat,
	FILENAME = 'C:\data\lab11dat.mdf', 
	SIZE = 10,
	MAXSIZE = UNLIMITED, 
	FILEGROWTH = 5%
)
LOG ON
(
	NAME = lab8_Log,
	FILENAME = 'C:\data\lab11log.ldf',
	SIZE = 5MB,
	MAXSIZE = 25MB,
	FILEGROWTH = 5MB
)
GO

USE lab11
GO

-- создание функций
DROP FUNCTION IF EXISTS dbo.CheckEmail;
GO

CREATE FUNCTION dbo.CheckEmail (@email NVARCHAR(80))
RETURNS BIT AS
BEGIN
    IF @email LIKE '%@%.%'
		RETURN 1
	RETURN 0
END;
GO

-- создание таблиц согласно ЛР4 + индексы
DROP TABLE IF EXISTS Client;
DROP TABLE IF EXISTS Coach;
DROP TABLE IF EXISTS Training;
DROP TABLE IF EXISTS Abonement;
DROP TABLE IF EXISTS AbonementType;
DROP TABLE IF EXISTS TrainingHistory;
GO


CREATE TABLE Client
(
	id INT IDENTITY(1,1) PRIMARY KEY,
    email NVARCHAR(80) UNIQUE NOT NULL CHECK (LEN(email) > 8 AND dbo.CheckEmail(email)=1),
    name NVARCHAR(50) NOT NULL,
    surname NVARCHAR(70) NOT NULL CHECK (LEN(surname) > 1) ,
	gender BIT NULL,
    dateOfBirth Date NULL CHECK (dateOfBirth < DATEADD(year, -14, GETDATE())),

	CONSTRAINT client_name_check CHECK (LEN(name) > 1)
);
GO

DROP INDEX IF EXISTS ClientNameIndex ON Client;
GO
CREATE INDEX ClientNameIndex
	ON Client (surname,name DESC)
	INCLUDE (email,dateOfBirth)
GO

CREATE TABLE Coach
(
	id INT IDENTITY(1,1) PRIMARY KEY,
    email NVARCHAR(80) UNIQUE NOT NULL CHECK (LEN(email) > 8 AND dbo.CheckEmail(email)=1),
    name NVARCHAR(50) NOT NULL,
    surname NVARCHAR(70) NOT NULL CHECK (LEN(surname) > 1) ,
	gender BIT NULL,
    dateOfBirth Date NULL CHECK (dateOfBirth < DATEADD(year, -18, GETDATE())),
	salary MONEY NOT NULL,
	specialization NVARCHAR(120) NULL DEFAULT 'Coach',

	CONSTRAINT coach_name_check CHECK (LEN(name) > 1)
);
GO

DROP INDEX IF EXISTS CoachNameIndex ON Client;
GO
CREATE INDEX CoachNameIndex
	ON Coach (surname,name DESC)
	INCLUDE (email,dateOfBirth)
GO

CREATE TABLE Training
(
	dateOfTraining date NOT NULL,
	startTime time(0) NOT NULL,
	endTime time(0) NOT NULL,
	coach_id INT FOREIGN KEY REFERENCES Coach(id) ON DELETE CASCADE,
	client_id INT FOREIGN KEY REFERENCES Client(id) ON DELETE CASCADE,
	PRIMARY KEY (dateOfTraining, coach_id,client_id)
);
GO

DROP INDEX IF EXISTS TrainingIndex ON Training;
GO
CREATE INDEX TrainingIndex
	ON Training (dateOfTraining,coach_id)
	INCLUDE (client_id)
GO

CREATE TABLE AbonementType
(
	id INT IDENTITY(1,1) PRIMARY KEY,
	name NVARCHAR(40) NOT NULL UNIQUE CHECK (LEN(name) > 1),
	price FLOAT NOT NULL CHECK (price >= 0),
	duration NVARCHAR(80) NOT NULL CHECK (duration IN('30 days', '1 year','90 days','7 days'))
);
GO

CREATE TABLE Abonement
(
	date_ Date NOT NULL DEFAULT GETDATE(),
	expires_at Date NULL CHECK (expires_at > DATEADD(year, -12, GETDATE())),
	abonement_type_name NVARCHAR(40) FOREIGN KEY REFERENCES AbonementType(name) ON DELETE CASCADE,
	client_id INT FOREIGN KEY REFERENCES Client(id) ON DELETE CASCADE,
	PRIMARY KEY (client_id,date_,abonement_type_name)
);
GO


-- создание триггеров
DROP TRIGGER IF EXISTS DeletePastAbonement;
DROP TRIGGER IF EXISTS InsertTraining;
DROP TRIGGER IF EXISTS InsertLuckyClient;
GO

CREATE TRIGGER DeletePastAbonement ON Abonement
AFTER INSERT, UPDATE, DELETE
AS
	DELETE FROM Abonement WHERE expires_at < GETDATE()
GO

CREATE TRIGGER InsertLuckyClient ON Client
AFTER INSERT
AS
	BEGIN
		DECLARE @type_name NVARCHAR(40)
		SET @type_name=(SELECT name FROM AbonementType WHERE name='Lucky')
		IF @type_name<>''
			INSERT INTO Abonement(date_,expires_at,abonement_type_name,client_id)
			SELECT GETDATE(), DATEADD(day,7,GETDATE()),  @type_name, id FROM inserted WHERE (id % 3=0)
		ELSE
			PRINT('Не установлен Lucky тип абонемента, если хотите подключить акцию, добавьте его в таблицу AbonementType')
	END
GO


CREATE TRIGGER InsertTraining ON Training
INSTEAD OF INSERT
AS
BEGIN
	IF EXISTS(SELECT 1 FROM Abonement INNER JOIN inserted ON inserted.client_id=Abonement.client_id WHERE expires_at<GETDATE())
		DELETE FROM Abonement WHERE expires_at < GETDATE()

	IF EXISTS(SELECT 1 FROM Abonement INNER JOIN inserted ON inserted.client_id=Abonement.client_id)
		INSERT INTO Training (dateOfTraining,startTime,endTime,client_id,coach_id)
		SELECT dateOfTraining,startTime,endTime,client_id,coach_id FROM inserted WHERE DATEDIFF(hour,endTime,startTime)<2
	ELSE
		THROW 51000, 'Невозможно создать тренировку. Абонемент истёк', 1;
END
GO

INSERT INTO AbonementType(name,price,duration) VALUES
('Lucky',0,'7 days'),
('Base',100,'30 days'),
('Base+',200,'30 days');

INSERT INTO Client(email, name, surname, gender,dateOfBirth) VALUES
('email@mail.ru','Oleg','Petrov',1,'2002-02-06'),
('email1@mail.ru','Daria','Petrova',0,'2000-02-06'),
('email2@mail.ru','Petr','Razumov',1,'2005-02-06'),
('email3@mail.ru','Nikolay','Stepanov',NULL,NULL);


INSERT INTO Coach(email, name, surname, gender,dateOfBirth,salary) VALUES
('coach@mail.ru','Semen','Ivanov',1,'2002-02-06',500),
('coach1@mail.ru','Daria','Vlasova',0,'2000-02-06',1000),
('coach2@mail.ru','Elina','Mamedova',0,'1999-02-06',1500),
('coach3@mail.ru','Maria','Kekova',0,'2005-02-06',1000);

INSERT INTO Abonement(date_,expires_at,abonement_type_name, client_id) VALUES
(CONVERT(date, GETDATE()), CONVERT(date, N'02-01-2021'),'Base',1),
(CONVERT(date, GETDATE()), CONVERT(date, N'03-01-2024'),'Base',2),
(CONVERT(date, GETDATE()), CONVERT(date, N'02-01-2024'),'Base+',3),
(CONVERT(date, GETDATE()), CONVERT(date, N'03-01-2024'),'Base+',2);

INSERT INTO Training(dateOfTraining,startTime,endTime,client_id,coach_id) VALUES
(GETDATE(),CONVERT(time(0),GETDATE()), CONVERT(time(0),DATEADD(hour,1,GETDATE())), 2,2),
(DATEADD(day,-1,GETDATE()),CONVERT(time(0),GETDATE()), CONVERT(time(0),DATEADD(hour,1,GETDATE())), 2,2),
(GETDATE(),CONVERT(time(0),GETDATE()), CONVERT(time(0),DATEADD(hour,2,GETDATE())), 1,3);


SELECT * FROM Training;
SELECT * FROM Client;
SELECT * FROM AbonementType;
SELECT * FROM Abonement;
SELECT * FROM Coach;


DELETE FROM Client WHERE email='email2@mail.ru';

SELECT * FROM Abonement;
SELECT * FROM Client;

UPDATE Training SET coach_id=1 WHERE coach_id=2 AND client_id=2;
SELECT * FROM Training;

--удаление повторяющихся записей (DISTINCT)
SELECT DISTINCT salary FROM Coach

--выбор, упорядочивание и именование полей
SELECT id as [Номер тренера в базе], email as Почта, surname as Фамилия, name as Имя FROM Coach

-- соединение таблиц
-- INNER JOIN
SELECT a.date_, a.expires_at, a_t.name, a_t.price FROM Abonement as a INNER JOIN AbonementType as a_t ON a.abonement_type_name=a_t.name
-- LEFT JOIN
SELECT a.date_, a.expires_at, a_t.name, a_t.price FROM Abonement as a LEFT JOIN AbonementType as a_t ON a.abonement_type_name=a_t.name
-- RIGHT JOIN
SELECT a.date_, a.expires_at, a_t.name, a_t.price FROM Abonement as a RIGHT JOIN AbonementType as a_t ON a.abonement_type_name=a_t.name
-- FULL OUTER JOIN
SELECT a.date_, a.expires_at, a_t.name, a_t.price FROM Abonement as a FULL OUTER JOIN AbonementType as a_t ON a.abonement_type_name=a_t.name

--условия выбора записей
--NULL
SELECT * FROM Client WHERE gender IS NULL
SELECT * FROM Client WHERE gender IS NOT NULL
--LIKE
SELECT * FROM Client WHERE surname LIKE '%ov'
-- BETWEEN
SELECT * FROM Coach WHERE salary BETWEEN 1000 AND 1500 ORDER BY salary DESC
-- IN
SELECT * FROM Client WHERE email IN ('email@mail.ru','email1@mail.ru')
-- EXISTS - не эффективны повторно запускается для КАЖДОЙ строки в таблице внешнего запроса
SELECT * FROM AbonementType WHERE EXISTS(SELECT * FROM Abonement WHERE Abonement.abonement_type_name=name)

-- сортировка записей (ORDER BY - ASC, DESC)
SELECT * FROM Coach ORDER BY salary
SELECT * FROM Coach ORDER BY salary ASC
SELECT * FROM Coach ORDER BY salary DESC

-- группировка записей
-- GROUP BY + HAVING: FROM-WHERE-GROUP-HAVING-SELECT
-- COUNT
SELECT gender, COUNT(*) as count FROM Coach GROUP BY gender
SELECT gender, COUNT(*) as count FROM Coach WHERE dateOfBirth>'2000-01-01' GROUP BY gender
--AVG
SELECT gender, AVG(salary) as avg_salary FROM Coach GROUP BY gender
SELECT gender, AVG(salary) as avg_salary FROM Coach GROUP BY gender HAVING AVG(salary)>1000
--SUM
SELECT specialization, SUM(salary) as sum_salary FROM Coach GROUP BY specialization
--MIN
SELECT gender, MIN(salary) as min_salary FROM Coach WHERE dateOfBirth>'2000-01-01' GROUP BY gender
SELECT gender, MIN(salary) as min_salary FROM Coach WHERE dateOfBirth>'2000-01-01' GROUP BY gender HAVING AVG(salary)<1000
--MAX
SELECT gender, MAX(salary) as max_salary FROM Coach WHERE dateOfBirth>'2000-01-01' GROUP BY gender
SELECT gender, MAX(salary) as max_salary FROM Coach WHERE dateOfBirth>'2000-01-01' GROUP BY gender HAVING AVG(salary)<1000

--объединение результатов нескольких запросов
-- UNION
SELECT * FROM Coach WHERE id BETWEEN 0 AND 3
UNION 
SELECT * FROM Coach WHERE id IN (4, 3)
ORDER BY id DESC
GO

-- UNION  ALL
SELECT * FROM Coach WHERE id BETWEEN 0 AND 3
UNION ALL
SELECT * FROM Coach WHERE id IN (4, 3)
ORDER BY id DESC
GO

-- EXCEPT
SELECT * FROM Coach WHERE id BETWEEN 0 AND 3
EXCEPT
SELECT * FROM Coach WHERE id IN (4, 3)
GO

-- INTERSECT
SELECT * FROM Coach WHERE id BETWEEN 0 AND 3
INTERSECT
SELECT * FROM Coach WHERE id IN (4, 3)
GO

--вложенные запросы
SELECT * FROM Coach WHERE salary > (SELECT salary FROM Coach WHERE email = 'coach3@mail.ru')