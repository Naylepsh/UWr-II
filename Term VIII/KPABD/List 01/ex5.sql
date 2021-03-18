select C.FirstName, C.LastName, SUM(DOH.UnitPriceDiscount * DOH.UnitPrice) from SalesLT.Customer as C
join SalesLT.SalesOrderHeader as SOH on SOH.CustomerID = C.CustomerID
join SalesLT.SalesOrderDetail as DOH on DOH.SalesOrderID = SOH.SalesOrderID
group by C.FirstName, C.LastName
