function divisibleByItsDigits(num) {
    return String(num).split('').every(digit => num % digit === 0);
}

function divisibleBySumOfDigits(num) {
    return String(num).split('').reduce((total, digit) => total + Number(digit), 0);
}

function foo(begin, end){
    let numbers = [];
    for (let number = begin; number < end; number++){
        if (divisibleByItsDigits(number) && divisibleBySumOfDigits(number)) {
            numbers.push(number);
        }
    }
    return numbers;
}

console.log(foo(1, 100000));