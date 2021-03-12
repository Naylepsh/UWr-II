select City, 
	COUNT(DISTINCT SalesLT.CustomerAddress.CustomerID) as NumberOfCustomers, 
	COUNT(DISTINCT SalesLT.Customer.SalesPerson) as NumberOfSalesPeople
from SalesLT.Address
join SalesLT.CustomerAddress on SalesLT.Address.AddressID = SalesLT.CustomerAddress.AddressID
join SalesLT.Customer on SalesLT.CustomerAddress.CustomerID = SalesLT.Customer.CustomerID
group by City