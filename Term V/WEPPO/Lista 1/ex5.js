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

function dateTimer(f, label){
    function wrapped(...args){
        const date = new Date();
        const result = f(args);
        console.log(label+':', new Date().getTime() - date.getTime(), 'ms');
        return result;
    }
    return wrapped;
}

function testFibTimes(timer){
    const timedFibIter = timer(fibIter, 'fibIter');
    const timedFibRec =  timer(fibRec, 'fibRec');
    for (let n = 10; n <= 35; n++){
        console.log('=================')
        console.log('Times for n =', n);
        timedFibIter(n);
        timedFibRec(n);
    }
}


// testFibTimes(consoleTimer);
// testFibTimes(dateTimer);