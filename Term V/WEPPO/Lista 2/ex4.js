
/*
skladnia instanceof: zmienna instanceof typ
instanceof sprawdza czy dana zmienna zostala zainicjonowana konstruktorem danego typu

ze stacka: 
he instanceof operator tests whether the prototype property of a constructor appears anywhere in the prototypes chain of an object. 
In most cases this mean that the object was created by using this constructor or on of its descendant. 
But also prototype may be set explicitly by Object.setPrototypeOf() method (ECMAScript 2015) 
or by the __proto__ property (old browsers, deprecated).
 Changing the prototype of an object is not recommended though, because of performance issues. 
*/

/*
skladnia typeof: typeof zmienna == 'nazwaTypu'
typeof zwraca typ obiektu, sprawdza czy obiekt jest typu prymitywnego czy jest czyms innym (obiektem)

ze stacka:
The typeof operator tests whether value belong to one of six basic types: 
"number", "string", "boolean", "object", "function" or "undefined".
Where the string "object" belong all objects (except functions, which are objects, but have its own value in typeof operator),
and also "null" value and arrays (for "null" it's a bug, but this bug is so old, so it's become a standard). 
It doesn't rely on constructors and can be used even if value is undefined. But it's doesn't give any details about objects. 
So if you needed it, go to instanceof.
*/

// instance of nie radzi sobie z 'primitive types'
'example string' instanceof String; // false
typeof 'example string' == 'string'; // true

'example string' instanceof Object; // false
typeof 'example string' == 'object'; // false

true instanceof Boolean; // false
typeof true == 'boolean'; // true

99.99 instanceof Number; // false
typeof 99.99 == 'number'; // true

function f() {}
f instanceof Function; // true
typeof function() {} == 'function'; // true

// instanceof radzi sobie z customowymi i trudniejszymi typami
var ClassFirst = function () {};
var ClassSecond = function () {};
var instance = new ClassFirst();
typeof instance; // object
typeof instance == 'ClassFirst'; // false
instance instanceof Object; // true
instance instanceof ClassFirst; // true
instance instanceof ClassSecond; // false 

/regularexpression/ instanceof RegExp; // true
typeof /regularexpression/; // object

[] instanceof Array; // true
typeof []; //object

// A good reason to use typeof is if the variable may be undefined.
alert(typeof undefinedVariable); // alerts the string "undefined"
alert(undefinedVariable instanceof Object); // throws an exception

// A good reason to use instanceof is if the variable may be null.
var myNullVar = null;
alert(typeof myNullVar ); // alerts the string "object"
alert(myNullVar  instanceof Object); // alerts "false"

// kolejne porownanie
const foo = new String('foo');
console.log(foo instanceof String);//true
console.log(typeof foo);//object

const bar = 'bar';
console.log(bar instanceof String);//false
console.log(typeof bar);//string