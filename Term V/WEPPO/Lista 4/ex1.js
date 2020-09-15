// ES2015
// class Tree {
//   constructor(value, left, right) {
//     this.value = value;
//     this.left = left;
//     this.right = right;
//   }

//   *[Symbol.iterator]() {
//     if (this.left !== null) {
//       for (let e of this.left){
//         yield e;
//       }
//     }
//     yield this.value;
//     if (this.right !== null) {
//       for (let e of this.right){
//         yield e;
//       }
//     }
//   }
// }

// Old js
function Tree(value, left, right) {
  this.value = value;
  this.left = left;
  this.right = right;
}

Tree.prototype[Symbol.iterator] = function*() {
  if (this.left !== null) {
    for (let e of this.left){
      yield e;
    }
  }
  yield this.value;
  if (this.right !== null) {
    for (let e of this.right){
      yield e;
    }
  }
}

let node1 = new Tree(1, null, null);
let node3 = new Tree(3, null, null);
let node2 = new Tree(2, node1, node3);

let node5 = new Tree(5, null, null);
let node7 = new Tree(7, null, null);
let node6 = new Tree(6, node5, node7);

let root = new Tree(4, node2, node6);
for (let node of root){
  console.log(node);
}