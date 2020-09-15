function forEach( a, f ) {
  if (typeof f !== 'function') {
    throw new TypeError();
  }

  const length = a.length;
  for (let i = 0; i < length; i++) {
    // if to skip empty elements i.e.: [1,2, ,3,4]
    if (i in a) {
      f(a[i], i, a);
    }
  }
}

function map( a, f ) {
  if (typeof f !== 'function') {
    throw new TypeError();
  }

  const length = a.length;
  newArr = [];
  for (let i = 0; i < length; i++) {
    if (i in a) {
      newArr.push(f(a[i], i, a));
    }
  }
  return newArr;
}

function filter( a, f ) {
  if (typeof f !== 'function') {
    throw new TypeError();
  }

  const length = a.length;
  newArr = [];
  for (let i = 0; i < length; i++) {
    if (i in a && f(a[i], i, a)) {
      newArr.push(a[i]);
    }
  }
  return newArr;
}

var a = [1,2, ,3,4];
forEach( a, _ => { console.log( _ ); } );
// [1,2,3,4]

function lesserThan3(x){
  return x < 3;
}

console.log(filter( a, _ => _ < 3 ));
console.log(filter(a, lesserThan3));
// [1,2]

function multiplyBy2(x){
  return x*2;
}
console.log(map( a, _ => _ * 2 ));
console.log(map( a, multiplyBy2 ));
// [2,4,6,8]

console.log([1,2,,3,4]);