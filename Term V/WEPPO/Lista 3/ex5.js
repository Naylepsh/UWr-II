function sum(...args) {
  return args.reduce((total, x) => total + x, 0);
}

console.log(sum(1,2,3));
// 6
console.log(sum(1,2,3,4,5));
// 15