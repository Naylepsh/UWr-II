-- DIRTY READS 
drop table if exists L4.Test
go

create table L4.Test(
    Id int identity primary key,
    val int,
)
go

insert into L4.Test values (1), (2), (3)
go

-- start here
begin tran t1
update L4.Test set val = 2 where Id = 1 -- run t2 after this
rollback tran t1
go

-- NONREPEATABLE READ
drop table if exists L4.Test
go

create table L4.Test(
    Id int identity primary key,
    val int,
)
go

insert into L4.Test values (1), (2), (3)
go

begin tran t1
update L4.Test set val = 2 where Id = 1
commit tran t1 -- go back to t2 after this
go

-- PHANTOM READS
drop table if exists L4.Test
go

create table L4.Test(
    Id int identity primary key,
    val int,
)
go

insert into L4.Test values (1), (2), (3)
go

-- start with t2
begin tran t1
insert into L4.Test values (4)
commit tran t1 -- go back to t2 after this
go
