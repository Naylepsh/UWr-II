-- SETUP
drop table if exists firstnames
create table firstnames (
	id int identity(1,1) not null primary key,
	firstname varchar(20)
)

drop table if exists lastnames
create table lastnames (
	id int identity(1,1) not null primary key,
	lastname varchar(20)
)

drop table if exists fldata
create table fldata(
	firstname varchar(20),
	lastname varchar(20),
	constraint pk primary key (firstname, lastname)
)

-- TEST DATA
insert into firstnames(firstname) values
('Jaime'),
('Ramachandra'),
('Darden'),
('Pooja'),
('Aslan'),
('Daphne'),
('Diodoros'),
('Ginny'),
('Zbyněk')

insert into lastnames(lastname) values
('Ramla'),
('Reino'),
('Enni'),
('Nebuchadnezzar'),
('Yeong-Ja'),
('Bozhidar'),
('Priscila'),
('Vera'),
('Sait')
go


-- PROCEDURE DECLARATION
drop procedure if exists fill_fldata
go

create procedure fill_fldata @n int
as
begin
	declare @possible_fl_keys table (firstname varchar(20), lastname varchar(20))
	declare @possible_fl_keys_count int

	-- all unique firstname-lastname combinations
	insert into @possible_fl_keys
	select distinct firstname, lastname 
	from firstnames, lastnames

	select @possible_fl_keys_count=COUNT(*)
	from @possible_fl_keys

	-- anything past error 50000 is reserved for user defined errors
	if (@n > @possible_fl_keys_count) throw 51000, '@n must not be greater than the number of combinations', 1

	delete from fldata;

	-- inserting n random unique firstname-lastname combinations
	insert into fldata
	select top (@n) firstname, lastname
	from @possible_fl_keys
	order by NEWID()
end
go


-- EXECUTION
exec fill_fldata @n=8

select * from fldata
