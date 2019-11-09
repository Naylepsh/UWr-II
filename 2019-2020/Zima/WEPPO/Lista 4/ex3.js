let foo = function() {
  function qux() {
    console.log('REEEEEE');
  }
  return {
    bar : function() {
      qux()
    }
  }
}

let o = foo();
console.log(o);
o.bar();
