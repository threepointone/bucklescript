// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("../runtime/caml_builtin_exceptions");
var Obj                     = require("./obj");
var Caml_curry              = require("../runtime/caml_curry");

var Undefined = {
  0: "CamlinternalLazy.Undefined",
  1: Caml_builtin_exceptions.get_id(),
  length: 2,
  tag: 248
};

function raise_undefined() {
  throw Undefined;
}

function force_lazy_block(blk) {
  var closure = blk[0];
  blk[0] = raise_undefined;
  try {
    var result = Caml_curry.app1(closure, /* () */0);
    blk[0] = result;
    blk.tag = Obj.forward_tag;
    return result;
  }
  catch (e){
    blk[0] = function () {
      throw e;
    };
    throw e;
  }
}

function force_val_lazy_block(blk) {
  var closure = blk[0];
  blk[0] = raise_undefined;
  var result = Caml_curry.app1(closure, /* () */0);
  blk[0] = result;
  blk.tag = Obj.forward_tag;
  return result;
}

function force(lzv) {
  var t = lzv.tag | 0;
  if (t === Obj.forward_tag) {
    return lzv[0];
  }
  else if (t !== Obj.lazy_tag) {
    return lzv;
  }
  else {
    return force_lazy_block(lzv);
  }
}

function force_val(lzv) {
  var t = lzv.tag | 0;
  if (t === Obj.forward_tag) {
    return lzv[0];
  }
  else if (t !== Obj.lazy_tag) {
    return lzv;
  }
  else {
    return force_val_lazy_block(lzv);
  }
}

exports.Undefined            = Undefined;
exports.force_lazy_block     = force_lazy_block;
exports.force_val_lazy_block = force_val_lazy_block;
exports.force                = force;
exports.force_val            = force_val;
/* No side effect */
