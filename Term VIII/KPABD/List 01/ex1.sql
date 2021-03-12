select distinct City from SalesLT.SalesOrderHeader
join SalesLT.Address on SalesLT.SalesOrderHeader.ShipToAddressID=SalesLT.Address.AddressID
order by City asc