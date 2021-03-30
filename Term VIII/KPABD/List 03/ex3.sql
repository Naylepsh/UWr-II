drop table if exists L3.Cache
create table L3.Cache (
	ID int identity primary key,
	UrlAddress varchar(255) unique,
	LastAccess datetime
)

drop table if exists L3.History
create table L3.History (
	ID int identity primary key,
	UrlAddress varchar(255) unique,
	LastAccess datetime
)

drop table if exists L3.Parameters
create table L3.Parameters (
	Name varchar(255),
	Value int
)
go

drop trigger if exists TR_Cache_Insert
go

drop trigger if exists TR_History_Insert
go

insert into L3.Parameters values ('max_cache', 2)

insert into 
	L3.Cache
values
	('foo.bar', cast('2006-12-30 00:38:54.840' as datetime))
go

create trigger TR_History_Insert
on L3.History
instead of insert
as
begin
	declare @UrlAddress varchar(255), @LastAccess datetime
	select @UrlAddress=UrlAddress, @LastAccess=LastAccess from inserted

	print 'HISTORY -- ' 
	print @UrlAddress

	if (select count(*) from L3.History where UrlAddress = @UrlAddress) = 1
		update L3.History set LastAccess = getdate() where UrlAddress = @UrlAddress
	else
		insert into L3.History values (@UrlAddress, @LastAccess)
end
go

create trigger TR_Cache_Insert
on L3.Cache
instead of insert
as
begin
	declare @UrlAddress varchar(255)
	declare C_Cache_Insert cursor static for select UrlAddress from inserted
	open C_Cache_Insert
	fetch next from C_Cache_Insert into @UrlAddress
	while (@@fetch_status = 0)
	begin
		print 'CACHE --'
		print @UrlAddress
		if (select count(*) from L3.Cache where UrlAddress = @UrlAddress) = 1
			update L3.Cache set LastAccess = getdate() where UrlAddress = @UrlAddress
		else
		begin
			-- if cache is full
			if (select Value from L3.Parameters where Name = 'max_cache') <= (select count(*) from L3.Cache)
			begin
				declare @OldEntryID int, @OldEntryUrl varchar(255), @OldEntryLastAccess datetime
				select top 1 
					@OldEntryID=ID, 
					@OldEntryUrl=UrlAddress, 
					@OldEntryLastAccess=LastAccess 
				from L3.Cache 
				order by LastAccess asc

				print 'OLD ENTRY--'
				print @OldEntryUrl

				delete from L3.Cache where ID = @OldEntryID

				insert into L3.History values (@OldEntryUrl, @OldEntryLastAccess)
			end

			insert into L3.Cache values (@UrlAddress, getdate())
		end

		fetch next from C_Cache_Insert into @UrlAddress
	end
	close C_Cache_Insert
	deallocate C_Cache_Insert
end
go

insert into L3.Cache (UrlAddress) values ('abc.de'), ('uwu.jp'), ('foo.bar'), ('awooo')
insert into L3.Cache (UrlAddress) values ('uwu.jp')

select * from L3.Cache
select * from L3.History
