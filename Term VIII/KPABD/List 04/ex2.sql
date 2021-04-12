﻿-- ORIGINAL
create table Appointment (
	ID int identity primary key,
	Patient varchar(255),
	PatientAddress varchar(255),
	AppointmentTimeAndLocation varchar(255),
	Price varchar(255),
	Physician varchar(255),
	AppointmentCause varchar(1023),
)

-- 1NF:
create table Appointment (
	ID int identity primary key,
	Patient varchar(255), -- could be: FirstName | LastName
	PatientAddress varchar(255), -- could be: Street | Number
	PatientPostalCode varchar(255),
	PatientCity varchar(255),
	Time datetime,
	Location varchar(255),
	Price decimal(10,2),
	PriceCurrency varchar(31),
	Physician varchar(255), -- could be: FirstName | LastName
	Category varchar(255),
	Cause varchar(1023)
)


-- 2NF
create table Patient (
	ID int identity primary key,
	Name varchar(255),
	Address varchar(255),
	PostalCode varchar(255),
	City varchar(255)
)

create table Appointment (
	ID int identity primary key,
	PatientID int,
	Time datetime,
	Location varchar(255),
	Price decimal(10,2),
	PriceCurrency varchar(31),
	Physician varchar(255),
	Category varchar(255),
	Cause varchar(1023)
)

-- 3NF:
--  Assuming that prices can be somewhat arbitrary, then the current schemas are in 3NF
-- However, if Causes are not arbitrary (for example, 
-- there won't be a case where one denture fitting is called "Denture fitting in ..." 
-- and the other "Needs to fit denture ..."), then:
create table Patient (
	ID int identity primary key,
	Name varchar(255),
	Address varchar(255),
	PostalCode varchar(255),
	City varchar(255)
)

create table Appointment (
	ID int identity primary key,
	PatientID int,
	Time datetime,
	Location varchar(255),
	Physician varchar(255),
	Category varchar(255),
	Cause varchar(1023)
)

create table AppointmentPrice (
	Category varchar(255),
	Cause varchar(1023),
	Price decimal(10,2),
	PriceCurrency varchar(31),
	constraint PK_AppointmentPrice primary key (Category, Cause)
)
-- If locations depends on Physician, then ...