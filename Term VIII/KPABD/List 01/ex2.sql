select PM.Name, COUNT(ProductID) from SalesLT.ProductModel as PM
join SalesLT.Product on PM.ProductModelID=SalesLT.Product.ProductModelID
group by PM.Name
having COUNT(ProductID) > 1