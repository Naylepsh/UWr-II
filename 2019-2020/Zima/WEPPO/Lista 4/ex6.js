const fs = require('fs');

function readFile(filename) {
  return fs.readFileSync(filename, encoding='utf-8', (err, data) => {
    if (!err) {
      return data;
    }
  });
}

console.log(readFile('ex5.js'));