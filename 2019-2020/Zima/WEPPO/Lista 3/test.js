function createFs(n) { 
  // tworzy tablicę n funkcji
  var fs = []; // i-ta funkcja z tablicy ma zwrócić i
  // w trakcie petli 'i' nie ma konkretnej wartosci
  for ( var i=0; i<n; i++ ) {
    fs[i] = function() {
      return i;
    }
  };
  return fs; // doperio teraz pod i wrzucana jest ostatnia wartosc jaka tam byla -- 10
}

var myfs = createFs(10);

console.log( myfs[0]() ); // zerowa funkcja miała zwrócić 0
console.log( myfs[2]() ); // druga miała zwrócić 2
console.log( myfs[7]() );