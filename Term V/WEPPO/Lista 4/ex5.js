let input = process.stdin;
input.setEncoding('utf-8');
console.log('Enter your name');
input.on('data', data => {
  console.log('Hello ' + data);
  process.exit();
});
