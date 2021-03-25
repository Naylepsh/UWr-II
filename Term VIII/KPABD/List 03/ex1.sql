drop table if exists L3.Prices
drop table if exists L3.Products
drop table if exists L3.Rates
go

create table L3.Products (
	ID int identity primary key,
	ProductName varchar(50)
)
go

create table L3.Rates (
	Currency varchar(3) unique,
	PricePLN decimal(10,2) not null
)
go

create table L3.Prices (
	ProductID int,
	Currency varchar(3),
	Price decimal(10,2) not null,
	constraint FkProduct foreign key (ProductID) references L3.Products (ID),
	constraint FkCurrency foreign key (Currency) references L3.Rates (Currency)
)
go

insert into 
	L3.Rates
values 
	('EUR', 4.5246),
	('GBP', 5.0218),
	('USD', 4.1127),
	('NOK', 0.4118),
	('CHF', 4.2675)
go

insert into 
	L3.Products (ProductName)
values 
	('Foo'),
	('Bar'),
	('Baz'),
	('Qux')
go

alter table L3.Prices nocheck constraint FkCurrency;
go

insert into 
	L3.Prices
values 
	(1, 'PLN', 5),
	(2, 'PLN', 3),
	(3, 'PLN', 2),
	(4, 'PLN', 6),
	(1, 'EUR', 5),
	(2, 'EUR', 4),
	(3, 'EUR', 10),
	(4, 'EUR', 1),
	(1, 'NOK', 1),
	(2, 'CHF', 1),
	(3, 'USD', 1)
go


-- Initial prices
select * from L3.Prices

delete from L3.Rates
where Currency = 'EUR'
go

declare cUpdatePrice cursor 
for 
	select 
		ProductID, 
		Price, 
		Currency 
	from L3.Prices
declare 
	@ProductID int, 
	@Price decimal(10,2), 
	@Currency varchar(3)
open cUpdatePrice
fetch next from cUpdatePrice into @ProductID, @Price, @Currency
while (@@FETCH_STATUS = 0)
begin
	-- remove the row if currency not recognised
	if (@Currency != 'PLN' and @Currency not in (select Currency from L3.Rates))
		delete from L3.Prices
		where current of cUpdatePrice
	-- update non-PLN price based on currency rates and prices in PLN
	else if (@Currency != 'PLN')
	begin
		declare @rate decimal(10,2)
		select @rate=PricePLN from L3.Rates

		declare @pricePLN decimal(10,2)
		select @pricePLN=Price from L3.Prices where Currency = 'PLN' and ProductID = @ProductID

		update L3.Prices
		set Price = @pricePLN * @rate
		where current of cUpdatePrice
	end

	fetch next from cUpdatePrice into @ProductID, @Price, @Currency
end
close cUpdatePrice
deallocate cUpdatePrice


-- Prices after correction
select * from L3.Prices
