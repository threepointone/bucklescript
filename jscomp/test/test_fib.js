// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_curry = require("../runtime/caml_curry");

function fib(n) {
  if (n === 0 || n === 1) {
    return 1;
  }
  else {
    return fib(n - 1) + fib(n - 2);
  }
}

function fib2(n) {
  if (n === 2 || n === 1) {
    return 1;
  }
  else {
    return fib2(n - 1) + fib2(n - 2);
  }
}

var v = 0;

for(var i = 0; i<= 10; ++i){
  v += i;
}

var sum = v;

var v$1 = 0;

for(var i$1 = 10; i$1>= 0; --i$1){
  v$1 += i$1;
}

var sumdown = v$1;

function cons(x, y) {
  return /* Cons */{
          0: x,
          1: y,
          length: 2,
          tag: 0
        };
}

function length(x) {
  if (x) {
    return 1 + length(x[1]);
  }
  else {
    return 0;
  }
}

function map(f, x) {
  if (x) {
    return /* Cons */{
            0: Caml_curry.app1(f, x[0]),
            1: map(f, x[1]),
            length: 2,
            tag: 0
          };
  }
  else {
    return /* Nil */0;
  }
}

function f(x) {
  var v = x;
  var sum = 0;
  while(v > 0) {
    sum += v;
    -- v;
  };
  return sum;
}

function fib3(n) {
  var _a = 0;
  var _b = 1;
  var _n = n;
  while(true) {
    var n$1 = _n;
    var b = _b;
    var a = _a;
    if (n$1 > 0) {
      _n = n$1 - 1;
      _b = a + b;
      _a = b;
      continue ;
      
    }
    else {
      return a;
    }
  };
}

var b = fib;

exports.fib     = fib;
exports.fib2    = fib2;
exports.b       = b;
exports.sum     = sum;
exports.sumdown = sumdown;
exports.cons    = cons;
exports.length  = length;
exports.map     = map;
exports.f       = f;
exports.fib3    = fib3;
/*  Not a pure module */
