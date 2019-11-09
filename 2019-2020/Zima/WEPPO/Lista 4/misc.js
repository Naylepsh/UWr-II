//1.
var Tree = function(left,right,val){
  this.left = left;
  this.right = right;
  this.val = val;
}

var testTree = new Tree( new Tree( new Tree (null, null, 3), null, 2 ), null, 1);
console.log(testTree)
console.log(testTree.left)

//2.
var Tree = function (left,right,val){
  this.left = left;
  this.right = right;
  this.val = val;
}

var iter = function(t){
  if (t == null){
      return [];
  } else{
      let t1 = [];
      let t2 = [];
      t1 = iter(t.left);
      t2 = iter(t.right);
      t1.unshift(t.val)
      return t1.concat(t2)
  }
}

/*
Tree.prototype[Symbol.iterator] = function* (){
  for (let e of this.left){
      yield e}
  yield this.val;
  for (let e of this.right){
      yield e;}
     
}
*/

Tree.prototype[Symbol.iterator] = function* (){
  let t = iter(this);
  for (let i = 0; i < t.length ; i++){
      yield t[i]
  }
}

var testTree = new Tree(new Tree(new Tree(null, null, 3),new Tree(null, null, 4), 2),
                      new Tree(new Tree(null, null, 6),new Tree(null, null, 7), 5), 1);


for (var e of testTree){
  console.log(e)
}

//3.


var foo = function() {
  var object = {
      bar : function (){
          private = 42;
          qux();
      },
      public : 10
  };
  var private = 42;
  function qux(){
      private = 42;
      object.public = 10;
      object.bar();
  }
  return object
}();

foo.bar().qux()

// 4
let module1 = require('./4.1');
module1.foo1(5);

// 4.1
module.exports = { foo1 };
let module2 = require('./4.2');

function foo1(n) {
  if ( n > 0 ) {
      console.log( `module1: ${n}`);
      module2.foo2(n-1);
  }
}

// 4.2

module.exports = { foo2 };
let module1 = require('./4.1');

function foo2(n) {
  if ( n > 0 ) {
      console.log( `module2: ${n}`);
      module1.foo1(n-1);
  }
}

// 5
process.stdin.setEncoding('utf8');
console.log("Podaj swoje imię")
var stdin = process.openStdin();
stdin.on('data', function(chunk) { console.log("Witaj " + chunk); });


// 6

var fs = require('fs');

fs.readFile('plik.txt', 'utf-8', function(err, data) {
console.log( data );
});

//7

var lineReader = require('readline').createInterface({
  input: require('fs').createReadStream('logs.txt')
});

function sortObject(obj) {
  var arr = [];
  for (var prop in obj) {
      if (obj.hasOwnProperty(prop)) {
          arr.push([prop,obj[prop]])
      }
  }
  arr.sort(function(a, b) { return a[1] - b[1]; });
  return arr;
}

var ips = {}
lineReader.on('line', function (line) {
  var ip = line.split(" ")[1];
  if (ip in ips){
      ips[ip] = ips[ip] + 1;
  } else{
      ips[ip] = 1
  }  
})
lineReader.on('close', function(){
  var sorted = sortObject(ips).reverse();
  console.log(sorted[0]);
  console.log(sorted[1]);
  console.log(sorted[2]);
})

// zad 8

var fs = require('fs');

//1

fs.readFile('plik.txt', 'utf-8', function(err, data1) {
  fs.readFile('plik2.txt', 'utf-8', function(err, data2) {
      fs.readFile('plik3.txt', 'utf-8', function(err, data3) {
          console.log(data1);
          console.log(data2);
          console.log(data3);
        });
    });
});

//2

function my_fspromise( path, enc ) {
  return new Promise( (res, rej) => {
      fs.readFile( path, enc, (err, data) => {
          if ( err )
              rej(err);
          res(data);
          });
      });
};

my_fspromise('plik.txt', 'utf-8')
  .then( data1 => {
      my_fspromise('plik2.txt', 'utf-8')
          .then( data2 => {
              my_fspromise('plik3.txt', 'utf-8')
                  .then( data3 => {
                      console.log( data1 );
                      console.log( data2 );
                      console.log( data3 );
              })
      })
  })

// 3

const util = require('util');
const readFile = util.promisify(fs.readFile);

readFile('plik.txt', 'utf8')
  .then((text) => {
      console.log(text);
  })
  .catch((err) => {
      console.log('Error', err);
  });

// 4

const fsPromises = require('fs').promises;
async function openAndClose() {
let file;
try {
  file = await fsPromises.readFile('plik.txt', 'utf-8');
  console.log(file)
} catch(e){
  console.log(e)
}
}
openAndClose();

// logs.txt

// 08:55:36 192.168.0.1 GET /TheApplication/WebResource.axd 200
// 08:55:36 192.168.0.2 GET /TheApplication/WebResource.axd 201
// 08:55:36 192.168.0.2 GET /TheApplication/WebResource.axd 202
// 08:55:36 192.168.0.2 GET /TheApplication/WebResource.axd 203
// 08:55:36 192.168.0.3 GET /TheApplication/WebResource.axd 204
// 08:55:36 192.168.0.1 GET /TheApplication/WebResource.axd 205
// 08:55:36 192.168.0.4 GET /TheApplication/WebResource.axd 206
// 08:55:36 192.168.0.5 GET /TheApplication/WebResource.axd 207
// 08:55:36 192.168.0.5 GET /TheApplication/WebResource.axd 208
// 08:55:36 192.168.0.6 GET /TheApplication/WebResource.axd 209
// 08:55:36 192.168.0.6 GET /TheApplication/WebResource.axd 210
// 08:55:36 192.168.0.6 GET /TheApplication/WebResource.axd 211
// 08:55:36 192.168.0.6 GET /TheApplication/WebResource.axd 212
// 08:55:36 192.168.0.8 GET /TheApplication/WebResource.axd 213
// 08:55:36 192.168.0.7 GET /TheApplication/WebResource.axd 214