// PODPUNKT 1
myObject = {
  foo: 'foo',
  bar: 'bar',
  spam: 'spam',
  'aaa': 'XD'
}

console.log('myObject["foo"] =', myObject['foo']);
index = 'foo';
console.log('myObject[index] =', myObject[index]);
console.log('myObject.foo =', myObject.foo);
/* roznice: 
przy [] mozemy uzywac stringa, albo zmiennej typu string
przy . musimy podac dokladnie nazwe pola -- nie moze tutaj juz byc zmienna
*/


// PODPUNKT 2
console.log('myObject[1] =', myObject[1]);
myObject[1] = 'myObject[1]IsHere';
console.log('myObject[1] =', myObject[1]);
console.log('myObject =', myObject);
/* zatem liczba w [] jest konwertowana na string,
ale nastepujace wywolania nie zadzialaja:
myObject.1,
myObject.'1'
*/
console.log(myObject['1'] === myObject[1]);

otherObject = {
  whatever: 'qweqwewq'
}

console.log('myObjecet[otherObject] =', myObject[otherObject]);
myObject[otherObject] = 6; // tak samo jak z 1 -- wpisane pod string -- '[object Object]'
// bo '[object Object]' === domyslny object.toString()
console.log('myObject =', myObject);

anotherObject = {
  a: 'b'
}
myObject[otherObject] = 7;
myObject[anotherObject] = 8;
console.log('=====')
console.log(myObject);

// PODPUNKT 3
arr = [0,1,2,3];
arr['0'] = 'stringHere';  // '0' (i jakakolwiek inna liczba) zostanie skonwerowane na int(0)
arr['test'] = 'testString'; // tak jak w obiekcie? [... test: 'testString']
arr[myObject] = 'myObjectHere';
arr[otherObject] = 'otherObjectHere'; // tak samo jak przy obiektach -- konwersja na string
console.log(arr);
console.log(arr.test, arr['test']); // tak mozna sie dostac do test
arr.length = 1; // tak, mozna -- wtedy pozostale elementy znikaja (oprocz par klucz-wartosc)
console.log(arr);
arr.length = 10; // tak tez mozna, nowe elementy beda 'empty item'
console.log(arr);