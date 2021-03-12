-- test data
insert into SalesLT.ProductCategory(ParentProductCategoryID, Name)
values (5, 'test')

select P.Name, PC.Name from SalesLT.Product as P
join SalesLT.ProductCategory as PC on PC.ProductCategoryID = P.ProductCategoryID
where PC.ProductCategoryID in (select ParentProductCategoryID from SalesLT.ProductCategory)

delete from SalesLT.ProductCategory
where ParentProductCategoryID = 5
