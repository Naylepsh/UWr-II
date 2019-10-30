function fibMemo(n) {
  let fibs = new Array(n+1);
  fibs[0] = 0;
  fibs[1] = 1;
  for (let i = 2; i < n; i++){
    fibs[i] = fibs[i-1] + fibs[i-2];
  }
  return fibs[n];
}

function fibIter(n){
  let fibs = [0, 1];
  for (let i = 0; i < n; i++){
      fibs[i%2] = fibs[i%2] + fibs[(i+1)%2];
  }
  return fibs[n%2];
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
  const timedFibRec =  timer(fibRec, 'fibRec');
  const timedFibMemo = timer(fibMemo, 'fibMemo');
  for (let n = 10; n <= 35; n++){
      console.log('=================')
      console.log('Times for n =', n);
      timedFibIter(n);
      timedFibRec(n);
      timedFibMemo(n);
  }
}

testFibTimes(consoleTimer);