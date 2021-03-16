alter table SalesLT.Customer
add CreditCardNumber varchar(16)

update SalesLT.Customer SET CreditCardNumber='' WHERE CreditCardNumber IS NULL

alter table SalesLT.Customer
alter column CreditCardNumber varchar(19) not null

alter table SalesLT.Customer
add constraint CreditCardNumberHasDigitsOnly check (CreditCardNumber not like '%[^0-9]%')
