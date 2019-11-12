function fibIter(n){
  let fibs = [0, 1];
  for (let i = 0; i < n; i++){
      fibs[i%2] = fibs[i%2] + fibs[(i+1)%2];
  }
  return fibs[n%2];
}

function memoize(fn) {
  let cache = {};

  return n => {
    if (n in cache) {
      return cache[n];
    } else {
      const res = fn(n);
      cache[n] = res;
      return res;
    }
  }
}


function fibRec(n){
  if (n == 0){
      return 0;
  } else if (n == 1){
      return 1;
  } else {
      return fibRec(n-1) + fibRec(n-2);
  }
}

//fibRec = memo(fibRec);

function consoleTimer(f, label){
  function wrapped(...args){
      console.time(label);
      const result = f(args);
      console.timeEnd(label);
      return result;
  }
  return wrapped;
}

function testFibTimes(timer){
  const timedFibIter = timer(fibIter, 'fibIter');
  const timedFibRec =  timer(memoize(fibRec), 'fibRec');
  console.log('first attempt');
  for (let n = 10; n <= 35; n++){
      console.log('=================')
      console.log('Times for n =', n);
      timedFibIter(n);
      timedFibRec(n);
      timedFibRec(n);
  }
}

testFibTimes(consoleTimer);