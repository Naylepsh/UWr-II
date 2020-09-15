function createGenerator(upTo) {
  return function() {
    var _state = 0;
    return {
      next : function() {
        return {
          value : _state,
          done : _state++ >= upTo
        }
      }
    }
  }
}

function range(end) {
  return {
    [Symbol.iterator] : createGenerator(end)
  }
}

for ( var f of range(10) )
  console.log(f);

console.log('#####');

for ( var f of range(20) )
  console.log(f);