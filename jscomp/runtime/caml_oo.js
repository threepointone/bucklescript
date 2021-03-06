// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("./caml_builtin_exceptions");
var Caml_array              = require("./caml_array");

var caml_methods_cache = Caml_array.caml_make_vect(1000, 0);

function caml_get_public_method(obj, tag, cacheid) {
  var meths = obj[0];
  var offs = caml_methods_cache[cacheid];
  if (meths[offs] === tag) {
    return meths[offs - 1];
  }
  else {
    var aux = function (_i) {
      while(true) {
        var i = _i;
        if (i < 3) {
          throw [
                Caml_builtin_exceptions.assert_failure,
                [
                  "caml_oo.ml",
                  43,
                  20
                ]
              ];
        }
        else if (meths[i] === tag) {
          caml_methods_cache[cacheid] = i;
          return i;
        }
        else {
          _i = i - 2;
          continue ;
          
        }
      };
    };
    return meths[aux(meths[0] * 2 + 1) - 1];
  }
}

exports.caml_get_public_method = caml_get_public_method;
/* No side effect */
