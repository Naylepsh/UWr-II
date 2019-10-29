console.log( (![]+[])[+[]] + (![]+[])[+!+[]] + ([![]]+[][[]])[+!+[]+[+[]]] + (![]+[])[!+[]+!+[]] );

/*
(![]+[])[+[]]

![] === false
false + [] === 'false'
+[] === 0
'false'[0] === 'f'
*/

/*
(![]+[])[+!+[]]

(![]+[]) === 'false'
+[] === 0
!0 === true
+true === 1
'false'[1] === 'a'
*/

/*
([![]]+[][[]])[+!+[]+[+[]]]

[![]] === [ false ]
[][[]] === undefined
[ false ] + undefined === 'falseundefined'
+!+[] === 1 // z poprzedniej czesci
[+[]] === [ 0 ] // tez z poprzedniej
1 + [ 0 ] === '10'
'falseundefined'[10] === i
*/

/*
(![]+[])[!+[] + !+[]]

(![]+[]) === 'false' // z poprzednich
!+[] === true // z poprzednich
true + true === 2
'false'[2] === l
*/

