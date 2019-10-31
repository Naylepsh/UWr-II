let o = {
  foo: 'foo',
  bar() {
    return 'bar';
  },
  get foo() {
    return foo;
  },
  set foo(value) {
    console.log('using foo setter');
    foo = value;
  }
}

o.foo = 5;

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

console.log(o); // 'property' doesnt show and neither does 'method'
console.log(o.property);
console.log(o.method2());

Object.defineProperty(o, 'spam', {
  get spam() { 
    console.log('using spam getter'); 
    return spam; 
  }
});
Object.defineProperty(o, 'spam', {
  set spam(value) { spam = value; }
});

console.log(o.spam);
o.spam = 10;
console.log(o);