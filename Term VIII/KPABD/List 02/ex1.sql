drop procedure if exists get_readers_keeping_specimen
go

create procedure get_readers_keeping_specimen @days int = 0 
as
select PESEL, COUNT(*) as specimens_number from Wypozyczenie
join Czytelnik on Czytelnik.Czytelnik_ID = Wypozyczenie.Czytelnik_ID
where DATEDIFF(day, Data, GETDATE()) >= @days
group by PESEL
go

exec get_readers_keeping_specimen @days=150
