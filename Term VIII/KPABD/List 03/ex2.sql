drop table if exists L3.Logs;
drop table if exists L3.SalaryHistory;
drop table if exists L3.Employees;
go

create table L3.Employees
(
    ID int identity primary key,
    SalaryGros decimal(19, 2),
)

create table L3.SalaryHistory
(
    ID int identity primary key,
    EmployeeID int,
    Year smallint,
    Month smallint,
    SalaryNet decimal(19, 2),
    SalaryGros decimal(19, 2),
    constraint FkEmployee foreign key (EmployeeID) references L3.Employees (ID),
    constraint OneRecordPerEmployeeMonthYear unique (EmployeeID, Year, Month),
)

create table L3.Logs
(
    ID int identity primary key,
    Description varchar(255),
    EmployeeID int,
    Month smallint,
    Year smallint,
);
go

insert into 
    L3.Employees
values 
	(10000),
	(2000),
	(5900),
	(3600),
	(4000)

insert into 
    L3.SalaryHistory
values 
	(1, 2021, 1, 10000, 10000 + 10000 * 0.18),
	(1, 2021, 2, 10000, 10000 + 10000 * 0.18),
	(1, 2021, 3, 10000, 10000 + 10000 * 0.18),
	(1, 2021, 4, 10000, 10000 + 10000 * 0.18),
	(1, 2021, 5, 10000, 10000 + 10000 * 0.18),
	(1, 2021, 6, 10000, 10000 + 10000 * 0.18),
	(1, 2021, 7, 10000, 10000 + 10000 * 0.18),
	(1, 2021, 8, 10000, 10000 + 10000 * 0.18)
	--(1, 2021, 8, 0, 0)

insert into 
    L3.SalaryHistory
values 
	(2, 2021, 1, 10000, 10000 + 10000 * 0.18),
	(2, 2021, 2, 10000, 10000 + 10000 * 0.18),
	(2, 2021, 3, 10000, 10000 + 10000 * 0.18),
	(2, 2021, 4, 10000, 10000 + 10000 * 0.18),
	(2, 2021, 5, 10000, 10000 + 10000 * 0.18),
	(2, 2021, 6, 10000, 10000 + 10000 * 0.18),
	(2, 2021, 7, 10000, 10000 + 10000 * 0.18),
	(2, 2021, 8, 8000, 8000 + 8000 * 0.18)

-- employee that started in february
insert into 
    L3.SalaryHistory
values 
	(3, 2021, 2, 5000, 5000 + 5000 * 0.18),
	(3, 2021, 3, 5000, 5000 + 5000 * 0.18),
	(3, 2021, 4, 5000, 5000 + 5000 * 0.18),
	(3, 2021, 5, 5000, 5000 + 5000 * 0.18),
	(3, 2021, 6, 5000, 5000 + 5000 * 0.18),
	(3, 2021, 7, 5000, 5000 + 5000 * 0.18),
	(3, 2021, 8, 5000, 5000 + 5000 * 0.18)
go

drop procedure if exists SP_CalculateEmployeesSalary
go

create procedure SP_CalculateEmployeesSalary @month int, @year int
as
begin
	declare C_CalculateEmployeesSalary cursor
	for 
	select 
		E.ID, 
		E.SalaryGros, 
		sum(SH.SalaryNet) as SalaryNetSum, 
		case 
			when 
				count(SH.Month) = max(SH.Month) - min(SH.Month) + 1 
			then 1 
			else 0 
		end as AreMonthsConsecutive,
		max(SH.Month)
	from L3.Employees E
	left join L3.SalaryHistory SH on E.ID = SH.EmployeeID and Month < @month and Year = @year
	group by E.ID, E.SalaryGros

	declare 
		@EmployeeID int, 
		@SalaryGros decimal(19, 2), 
		@SalaryNetSum decimal(19, 2), 
		@AreMonthsConsecutive bit,
		@LastPaidMonth smallint,
		@TaxThreshold int,
		@Salary decimal(19, 2),
		@BelowThresholdTaxValue decimal(3, 2),
		@AboveOrEqualToThresholdTaxValue decimal(3, 2)
	set @TaxThreshold = 85528
	set @BelowThresholdTaxValue = 0.18
	set @AboveOrEqualToThresholdTaxValue = 0.32

	open C_CalculateEmployeesSalary

	fetch next from C_CalculateEmployeesSalary 
	into @EmployeeID, @SalaryGros, @SalaryNetSum, @AreMonthsConsecutive, @LastPaidMonth

	while (@@FETCH_STATUS = 0)
	begin
		if @AreMonthsConsecutive = 0 or @LastPaidMonth != @month - 1
			insert into L3.Logs 
			values (
				'Salary computation error. Missing at least one consecutive month', 
				@EmployeeID, @month, @year)
		else if @SalaryNetSum >= @TaxThreshold
			begin
				set @Salary = @SalaryGros * (1 - @AboveOrEqualToThresholdTaxValue)

				insert into L3.SalaryHistory
				values (@EmployeeID, @year, @month, @Salary, @SalaryGros)
			end
		else if @SalaryNetSum + @SalaryGros * (1 - @BelowThresholdTaxValue) > @TaxThreshold
			begin
				declare @BelowTax decimal(19, 2), @AboveTax decimal(19, 2)
				set @BelowTax = (@TaxThreshold - @SalaryNetSum) * (1 - @BelowThresholdTaxValue)
				set @AboveTax = (@SalaryNetSum + @SalaryGros - @TaxThreshold) * (1 - @AboveOrEqualToThresholdTaxValue)
				set @Salary = @BelowTax + @AboveTax

				insert into L3.SalaryHistory
				values (@EmployeeID, @year, @month, @Salary, @SalaryGros)
			end
		else
			begin
				set @Salary = @SalaryGros * (1 - @BelowThresholdTaxValue)

				insert into L3.SalaryHistory
				values (@EmployeeID, @year, @month, @Salary, @SalaryGros)
			end
		fetch next from C_CalculateEmployeesSalary 
		into @EmployeeID, @SalaryGros, @SalaryNetSum, @AreMonthsConsecutive, @LastPaidMonth
	end

	close C_CalculateEmployeesSalary
	deallocate C_CalculateEmployeesSalary
end
go

exec SP_CalculateEmployeesSalary 9, 2021
exec SP_CalculateEmployeesSalary 10, 2021

select * from L3.Logs
select * from L3.SalaryHistory where month >= 9 and year = 2021

