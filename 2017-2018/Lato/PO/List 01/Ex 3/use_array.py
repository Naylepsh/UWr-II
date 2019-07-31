from Array import *


arr = [None, None]
insert(arr, 2, 2)
insert(arr, -2, -2)
insert(arr, -1, -1)
insert(arr, 1, 1)
print(arr)
for i in range(arr[0], arr[-1]+1):
    print(get_val(arr, i), end=" ")
print("\n")

arr2 = [None, None]
insert(arr2, -3, -3)
insert(arr2, 2, 2)
insert(arr2, 10, 4)
print(arr2)
for i in range(arr2[0], arr2[-1]+1):
    print(get_val(arr2, i), end=" ")
print("\n")

arr3 = []
insert(arr3, 1, 1)
insert(arr3, 2, 2)
insert(arr3, 0, 0)
print(arr3)




"""
# przyklad dla ktorego zapisujemy wskaznik danej wartosci do tablicy
class Class:
    def __init__(self):
        self.x = 0

    def change(self, x):
        self.x = x

    def get_x(self):
        return self.x


obj1 = Class()
arr3 = [None, None]
insert(arr3, obj1, 0)
print(get_val(arr3, 0).get_x()) # zwroci 0
obj1.change(3)
print(get_val(arr3, 0).get_x()) # zwroci 3
"""
