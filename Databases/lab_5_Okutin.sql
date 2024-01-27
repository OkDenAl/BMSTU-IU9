USE master;
GO

-- 1. —оздать базу данных (CREATE DATABASEЕ, определение настроек размеров файлов).
IF DB_ID (N'lab5') IS NOT NULL
DROP DATABASE lab5;
GO

CREATE DATABASE lab5
ON
(
	NAME = lab5dat,
	FILENAME = 'C:\data\lab5dat.mdf', 
	SIZE = 10,
	MAXSIZE = UNLIMITED, 
	FILEGROWTH = 5%
)
LOG ON
(
	NAME = lab5Log,
	FILENAME = 'C:\data\lab5log.ldf',
	SIZE = 5MB,
	MAXSIZE = 25MB,
	FILEGROWTH = 5MB
)
GO

-- 2. —оздать произвольную таблицу (CREATE TABLEЕ).
USE lab5
GO
IF OBJECT_ID(N'Client') is NOT NULL
	DROP TABLE Client;
GO

CREATE TABLE Client
(
	id INT PRIMARY KEY,
    email NVARCHAR(80) UNIQUE NOT NULL,
    name NVARCHAR(50) NOT NULL,
    surname NVARCHAR(70) NOT NULL,
	gender BIT NOT NULL,
    dateOfBirth Date NOT NULL
);
GO

SELECT * FROM Client;
GO

-- 3. ƒобавить файловую группу и файл данных (ALTER DATABASEЕ).
ALTER DATABASE lab5
ADD FILEGROUP LargeFileGroup;  
GO

ALTER DATABASE lab5
ADD FILE(
    NAME = lab5_LargeData,  
    FILENAME = 'C:\data\lab5dat2.ndf',  
    SIZE = 5MB,  
    MAXSIZE = 25MB,  
    FILEGROWTH = 5% 
)
TO FILEGROUP LargeFileGroup
GO


-- 4. —делать созданную файловую группу файловой группой по умолчанию.
ALTER DATABASE lab5
	MODIFY FILEGROUP LargeFileGroup DEFAULT;
GO

-- 5. —оздать еще одну произвольную таблицу
IF OBJECT_ID(N'Coach') is NOT NULL
	DROP TABLE Coach;
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
	specialization NVARCHAR(120)
);
GO

SELECT * FROM Coach;
GO

-- 6. ”далить созданную вручную файловую группу.
ALTER DATABASE lab5
	MODIFY FILEGROUP [primary] DEFAULT;
GO

DROP TABLE Coach
GO

ALTER DATABASE lab5
	REMOVE FILE lab5_LargeData;
GO
ALTER DATABASE lab5
	REMOVE FILEGROUP LargeFileGroup;
GO

-- 7. —оздать схему, переместить в нее одну из таблиц, удалить схему.
IF SCHEMA_ID(N'GymSchema') is NOT NULL
	DROP SCHEMA GymSchema;
GO

CREATE SCHEMA GymSchema;
GO


ALTER SCHEMA GymSchema 
	TRANSFER Client;
GO

if OBJECT_ID(N'GymSchema.Client') is NOT NULL
	DROP TABLE GymSchema.Client;
GO


DROP SCHEMA GymSchema;
GO