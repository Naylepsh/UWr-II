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

function *fibGen() {
  let _x = 0;
  let _y = 1;
  while (true){
    yield _x;
    [_x, _y] = [_y, _x + _y];
  }
}

var _it = fibIter();
for ( var _result; _result = _it.next(), !_result.done; ) {
  console.log( _result.value );
  if (_result.value > 50) {
    break;
  }
}

// can't iterate over iterator this way (without [Symbol.iterate] thingy)
for ( var i of fibGen() ) {
  console.log( i );
  if (i > 50) {
    break;
  }
}