let o = {
  foo: 'foo',
  bar() {
    return 'bar';
  },
  get getFoo() {
    return this.foo;
  },
  set setFoo(value) {
    console.log('using foo setter');
    this.foo = value;
  }
}

o.setFoo = 'XD';
// new property
o.spam = 'spam';

Object.defineProperty(o, 'property', {
  value: 42
});

// new method
o.method1 = () => { return 42; }

Object.defineProperty(o, 'method2', {
  value: function() {
    return 42;
  }
});

console.log(o); // 'property' doesnt show and neither does 'method1'
console.log(o.property);
console.log(o.method2());

Object.defineProperty(o, 'getSpam', {
  get() { 
    console.log('using spam getter'); 
    return this.spam; 
  }
});
Object.defineProperty(o, 'setSpam', {
  value: function(value) { 
      console.log('using setter');
      this.spam = value; 
    },
    writable: false
  }
);

console.log(o.getSpam);
o.setSpam(10);
o.spam = 3;
console.log(o);
