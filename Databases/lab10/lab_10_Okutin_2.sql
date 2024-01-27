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
