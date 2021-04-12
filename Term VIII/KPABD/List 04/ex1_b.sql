-- DIRTY READS
-- allows dirty reads
set transaction isolation level read uncommitted
go

-- does not allow dirty reads
set transaction isolation level read committed
go

-- start with t1
begin tran t2
select * from L4.Test where Id = 1
commit tran t2 -- rollback t1 after this

-- NONREPEATABLE READS
-- allow nonrepeatable reads
set transaction isolation level read uncommitted
go
set transaction isolation level read committed
go

-- does not allow nonrepeatable reads
set transaction isolation level repeatable read
go

-- start here
begin tran t2
select * from L4.Test where Id = 1 -- run t1 after this
select * from L4.Test where Id = 1
commit tran t2

-- PHANTOM READS
-- allow nonrepeatable reads
set transaction isolation level read uncommitted
go
set transaction isolation level read committed
go
set transaction isolation level repeatable read
go

-- does not allow nonrepeatable reads
set transaction isolation level snapshot
go

-- start here
begin tran t2
select sum(val) from L4.Test -- run t1 after this
select sum(val) from L4.Test 
commit tran t2
