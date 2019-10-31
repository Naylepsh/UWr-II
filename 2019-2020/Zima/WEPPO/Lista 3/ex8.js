function* take(it, top) {
  for (top; top > 0; top--){
    yield it.next().value;
  }
}

function *fibGen() {
  let _x = 0;
  let _y = 1;
  while (true){
    yield _x;
    [_x, _y] = [_y, _x + _y];
  }
}

function fibIter() {
  let _x = 0;
  let _y = 1;
  return {
    next : function() {
      let _current = _x;
      _x = _y;
      _y = _current + _x;
      return {
        value : _current,
        done  : false
      }
    }
  }
}

for (let num of take( fibGen(), 10 ) ) {
  console.log(num);
}

console.log('#####');

for (let num of take( fibIter(), 10 ) ) {
  console.log(num);
}