def rhombus(n):
	for i in range(n):
		print((n-i)*' ' + '#' + 2*i*'#')
	print((2*n+1)*'#')
	for i in range(n-1,-1,-1):
		print((n-i)*' ' + '#' + 2*i*'#')


rhombus(4)