const fs = require('fs');

// 0


function defaultAsync() {
  console.log('-------default');
  fs.readFile('ex3.js', 'utf-8', (err, data) => {
    if (err) {
      console.log(err);
    } else {
      console.log(data);
    }
  });
}

// 1
function myPromise(path, encoding) {
  return new Promise( (resolve, reject) => {
    fs.readFile(path, encoding, (err, data) => {
      if (err) {
        reject(err);
      } else {
        resolve(data);
      }
    });
  });
}

function withMyPromise() {
  console.log('----------With myPromise');
  fsPromise('ex3.js', 'utf-8')
  .then( data => {
    console.log(data);
  });
}

// 2
const promisify = require('util').promisify;
const readFile = promisify(fs.readFile);

function withPromisify() {
  console.log('--------with promisify');
  readFile('ex3.js', 'utf-8')
  .then( data => {
    console.log(data);
  })
  .catch( err =>{
    console.log(err);
  });
}

// 4
const fsPromises = fs.promises;
async function withFsPromises() {
  try {
    const data = await fsPromises.readFile('ex3.js', 'utf-8');
    console.log(data);
  } catch (err) {
    console.log(err);
  }
}
