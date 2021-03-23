-- SETUP
drop procedure if exists reader_borrowed_specimen
go

drop type if exists ids_table
create type ids_table
as table (id int)
go

-- Don't know whether we're supposed to use Liczba_Dni or Data to count the number of days
-- that the specimen has been kept
create procedure reader_borrowed_specimen @ids ids_table readonly
as
--select id as reader_id, SUM(Liczba_Dni) as sum_days
select id as reader_id, SUM(DATEDIFF(day, Data, GETDATE())) as sum_days
from @ids I
join Czytelnik on Czytelnik.Czytelnik_ID = I.id
join Wypozyczenie on Wypozyczenie.Czytelnik_ID = Czytelnik.Czytelnik_ID
group by id
go


-- EXEC
declare @czytelnik_ids as ids_table
insert into @czytelnik_ids
select Czytelnik_ID from Czytelnik

exec reader_borrowed_specimen @czytelnik_ids

