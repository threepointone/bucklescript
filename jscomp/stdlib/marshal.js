// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("../runtime/caml_builtin_exceptions");
var Caml_primitive          = require("../runtime/caml_primitive");
var Caml_string             = require("../runtime/caml_string");

function to_buffer(buff, ofs, len, v, flags) {
  if (ofs < 0 || len < 0 || ofs > buff.length - len) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Marshal.to_buffer: substring out of bounds"
        ];
  }
  else {
    return Caml_primitive.caml_output_value_to_buffer(buff, ofs, len, v, flags);
  }
}

var header_size = 20;

function data_size(buff, ofs) {
  if (ofs < 0 || ofs > buff.length - header_size) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Marshal.data_size"
        ];
  }
  else {
    return Caml_primitive.caml_marshal_data_size(buff, ofs);
  }
}

function total_size(buff, ofs) {
  return header_size + data_size(buff, ofs);
}

function from_bytes(buff, ofs) {
  if (ofs < 0 || ofs > buff.length - header_size) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Marshal.from_bytes"
        ];
  }
  else {
    var len = Caml_primitive.caml_marshal_data_size(buff, ofs);
    if (ofs > buff.length - (header_size + len)) {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Marshal.from_bytes"
          ];
    }
    else {
      return Caml_primitive.caml_input_value_from_string(buff, ofs);
    }
  }
}

function from_string(buff, ofs) {
  return from_bytes(Caml_string.bytes_of_string(buff), ofs);
}

function to_channel(prim, prim$1, prim$2) {
  return Caml_primitive.caml_output_value(prim, prim$1, prim$2);
}

function from_channel(prim) {
  return Caml_primitive.caml_input_value(prim);
}

exports.to_channel   = to_channel;
exports.to_buffer    = to_buffer;
exports.from_channel = from_channel;
exports.from_bytes   = from_bytes;
exports.from_string  = from_string;
exports.header_size  = header_size;
exports.data_size    = data_size;
exports.total_size   = total_size;
/* No side effect */
