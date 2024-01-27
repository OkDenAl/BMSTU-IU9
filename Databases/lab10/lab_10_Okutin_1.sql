-- 1 ����������� � ����������������� �� ��������
-- ��������� ������ �������� ���������� MS SQL Server,
-- ��������������� � ��������������
-- ���������� SET TRANSACTION ISOLATION LEVEL

USE lab10;
GO

-- 1) ������������� ������ (read uncommited)

/*SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED;
BEGIN TRANSACTION;
	SELECT * FROM Client;
	WAITFOR DELAY '00:00:05';
	SELECT * FROM Client;
COMMIT TRANSACTION;
GO*/

-- 2) ����������� ������ (read commited)


/*SET TRANSACTION ISOLATION LEVEL READ COMMITTED;
BEGIN TRANSACTION;
    SELECT * FROM Client;
	WAITFOR DELAY '00:00:05';
	SELECT * FROM Client;
COMMIT TRANSACTION;
GO*/


-- 3) ��������������� ������ (repeatable read)

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