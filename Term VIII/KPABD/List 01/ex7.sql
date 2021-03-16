-- SETUP -----
drop table if exists m1
create table m1 (
	k integer primary key,
	v varchar(20)
)

drop table if exists s1
create table s1 (
	k integer primary key,
	mfk int foreign key references m1(k)
)

drop table if exists m2
create table m2 (
	k1 integer,
	k2 integer,
	v varchar(20),
	constraint pk_m2 primary key (k1, k2)
)

drop table if exists s2
create table s2 (
	k integer primary key,
	mk1 integer,
	mk2 integer,
	constraint mfk foreign key (mk1, mk2) references m2(k1, k2)
)

insert into m2 (k1, k2, v)
values (1, 1, 'test1')
insert into m2 (k1, k2, v)
values (2, 2, 'test2')
insert into m2 (k1, k2, v)
values (3, 3, 'test3')

insert into s2 (k, mk1, mk2)
values (1, 1, 1)
insert into s2 (k, mk1, mk2)
values (2, 2, 2)
insert into s2 (k, mk1, mk2)
values (3, 3, 3)

-- TESTS ----
-- duplicate primary key -- should fail
insert into m2 (k1, k2, v)
values (1, 1, 'test value')


-- duplicate primary key -- should fail
insert into s2 (k, mk1, mk2)
values (1, 1, 1)

-- references non-existing keys -- should fail
insert into s2 (k, mk1, mk2)
values (42, 42, 42)


-- ON DELETE POLICIES ----
-- deleting parent record will fail with current foreign key policy (no action is default policy)
delete from m2
where k1 = 1

-- on delete set null policy
alter table s2
drop constraint mfk

alter table s2
add constraint mfk foreign key (mk1, mk2) references m2(k1, k2) on delete set null

delete from m2
where k1 = 1

select * from m2
select * from s2

-- on delete cascade policy
alter table s2
drop constraint mfk

alter table s2
add constraint mfk foreign key (mk1, mk2) references m2(k1, k2) on delete cascade

delete from m2
where k1 = 2

select * from m2
select * from s2

-- TEARDOWN ----
drop table s2
drop table m2
drop table s1
drop table m1
