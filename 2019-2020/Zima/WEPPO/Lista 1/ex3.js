function isPrime(x){
    for (let i = 2; i*i <= x; i++){
        if (x % i == 0){
            return false;
        }
    }
    return true;
}

function primesInRange(begin, end){
    let primes = [];
    for (let x = begin; x <= end; x++){
        if (isPrime(x)){
            primes.push(x);
        }
    }
    return primes;
}

console.log(primesInRange(2, 100000));