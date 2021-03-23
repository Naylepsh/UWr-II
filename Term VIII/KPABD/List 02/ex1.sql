drop procedure if exists get_readers_keeping_specimen
go

---- variant 1
---- users that keep at least one speciment for longer than @days days
---- together with all the speciments that they keep longer than @days days
--create procedure get_readers_keeping_specimen @days int = 0 
--as
--select PESEL, COUNT(*) as specimens_number from Wypozyczenie
--join Czytelnik on Czytelnik.Czytelnik_ID = Wypozyczenie.Czytelnik_ID
--where DATEDIFF(day, Data, GETDATE()) >= @days
--group by PESEL
--go


---- variant 2
---- users that keep at least one speciment for longer than @days days
---- together with all the speciments that they keep 
create procedure get_readers_keeping_specimen @days int = 0 
as
select PESEL, COUNT(*) as specimens_number from (
	-- readers that keep at least one specimen for longer than @days days
	select distinct PESEL, Czytelnik.Czytelnik_ID from Wypozyczenie
	join Czytelnik on Czytelnik.Czytelnik_ID = Wypozyczenie.Czytelnik_ID
	where DATEDIFF(day, Data, GETDATE()) >= @days) as temp
join Wypozyczenie on Wypozyczenie.Czytelnik_ID = temp.Czytelnik_ID 
group by PESEL
go


exec get_readers_keeping_specimen @days=150
