// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("../runtime/caml_builtin_exceptions");
var Caml_obj                = require("../runtime/caml_obj");
var Caml_curry              = require("../runtime/caml_curry");

function height(param) {
  if (param) {
    return param[4];
  }
  else {
    return 0;
  }
}

function create(l, x, d, r) {
  var hl = height(l);
  var hr = height(r);
  return /* Node */{
          0: l,
          1: x,
          2: d,
          3: r,
          4: hl >= hr ? hl + 1 : hr + 1,
          length: 5,
          tag: 0
        };
}

function singleton(x, d) {
  return /* Node */{
          0: /* Empty */0,
          1: x,
          2: d,
          3: /* Empty */0,
          4: 1,
          length: 5,
          tag: 0
        };
}

function bal(l, x, d, r) {
  var hl = l ? l[4] : 0;
  var hr = r ? r[4] : 0;
  if (hl > hr + 2) {
    if (l) {
      var lr = l[3];
      var ld = l[2];
      var lv = l[1];
      var ll = l[0];
      if (height(ll) >= height(lr)) {
        return create(ll, lv, ld, create(lr, x, d, r));
      }
      else if (lr) {
        return create(create(ll, lv, ld, lr[0]), lr[1], lr[2], create(lr[3], x, d, r));
      }
      else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Map.bal"
            ];
      }
    }
    else {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Map.bal"
          ];
    }
  }
  else if (hr > hl + 2) {
    if (r) {
      var rr = r[3];
      var rd = r[2];
      var rv = r[1];
      var rl = r[0];
      if (height(rr) >= height(rl)) {
        return create(create(l, x, d, rl), rv, rd, rr);
      }
      else if (rl) {
        return create(create(l, x, d, rl[0]), rl[1], rl[2], create(rl[3], rv, rd, rr));
      }
      else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Map.bal"
            ];
      }
    }
    else {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Map.bal"
          ];
    }
  }
  else {
    return /* Node */{
            0: l,
            1: x,
            2: d,
            3: r,
            4: hl >= hr ? hl + 1 : hr + 1,
            length: 5,
            tag: 0
          };
  }
}

function is_empty(param) {
  if (param) {
    return /* false */0;
  }
  else {
    return /* true */1;
  }
}

function add(x, data, param) {
  if (param) {
    var r = param[3];
    var d = param[2];
    var v = param[1];
    var l = param[0];
    var c = Caml_obj.caml_int_compare(x, v);
    if (c) {
      if (c < 0) {
        return bal(add(x, data, l), v, d, r);
      }
      else {
        return bal(l, v, d, add(x, data, r));
      }
    }
    else {
      return /* Node */{
              0: l,
              1: x,
              2: data,
              3: r,
              4: param[4],
              length: 5,
              tag: 0
            };
    }
  }
  else {
    return /* Node */{
            0: /* Empty */0,
            1: x,
            2: data,
            3: /* Empty */0,
            4: 1,
            length: 5,
            tag: 0
          };
  }
}

function find(x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var c = Caml_obj.caml_int_compare(x, param[1]);
      if (c) {
        _param = c < 0 ? param[0] : param[3];
        continue ;
        
      }
      else {
        return param[2];
      }
    }
    else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
}

function mem(x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var c = Caml_obj.caml_int_compare(x, param[1]);
      if (c) {
        _param = c < 0 ? param[0] : param[3];
        continue ;
        
      }
      else {
        return /* true */1;
      }
    }
    else {
      return /* false */0;
    }
  };
}

function min_binding(_param) {
  while(true) {
    var param = _param;
    if (param) {
      var l = param[0];
      if (l) {
        _param = l;
        continue ;
        
      }
      else {
        return /* tuple */[
                param[1],
                param[2]
              ];
      }
    }
    else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
}

function max_binding(_param) {
  while(true) {
    var param = _param;
    if (param) {
      var r = param[3];
      if (r) {
        _param = r;
        continue ;
        
      }
      else {
        return /* tuple */[
                param[1],
                param[2]
              ];
      }
    }
    else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
}

function remove_min_binding(param) {
  if (param) {
    var l = param[0];
    if (l) {
      return bal(remove_min_binding(l), param[1], param[2], param[3]);
    }
    else {
      return param[3];
    }
  }
  else {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Map.remove_min_elt"
        ];
  }
}

function remove(x, param) {
  if (param) {
    var r = param[3];
    var d = param[2];
    var v = param[1];
    var l = param[0];
    var c = Caml_obj.caml_int_compare(x, v);
    if (c) {
      if (c < 0) {
        return bal(remove(x, l), v, d, r);
      }
      else {
        return bal(l, v, d, remove(x, r));
      }
    }
    else {
      var t1 = l;
      var t2 = r;
      if (t1) {
        if (t2) {
          var match = min_binding(t2);
          return bal(t1, match[0], match[1], remove_min_binding(t2));
        }
        else {
          return t1;
        }
      }
      else {
        return t2;
      }
    }
  }
  else {
    return /* Empty */0;
  }
}

function iter(f, _param) {
  while(true) {
    var param = _param;
    if (param) {
      iter(f, param[0]);
      Caml_curry.app2(f, param[1], param[2]);
      _param = param[3];
      continue ;
      
    }
    else {
      return /* () */0;
    }
  };
}

function map(f, param) {
  if (param) {
    var l$prime = map(f, param[0]);
    var d$prime = Caml_curry.app1(f, param[2]);
    var r$prime = map(f, param[3]);
    return /* Node */{
            0: l$prime,
            1: param[1],
            2: d$prime,
            3: r$prime,
            4: param[4],
            length: 5,
            tag: 0
          };
  }
  else {
    return /* Empty */0;
  }
}

function mapi(f, param) {
  if (param) {
    var v = param[1];
    var l$prime = mapi(f, param[0]);
    var d$prime = Caml_curry.app2(f, v, param[2]);
    var r$prime = mapi(f, param[3]);
    return /* Node */{
            0: l$prime,
            1: v,
            2: d$prime,
            3: r$prime,
            4: param[4],
            length: 5,
            tag: 0
          };
  }
  else {
    return /* Empty */0;
  }
}

function fold(f, _m, _accu) {
  while(true) {
    var accu = _accu;
    var m = _m;
    if (m) {
      _accu = Caml_curry.app3(f, m[1], m[2], fold(f, m[0], accu));
      _m = m[3];
      continue ;
      
    }
    else {
      return accu;
    }
  };
}

function for_all(p, _param) {
  while(true) {
    var param = _param;
    if (param) {
      if (Caml_curry.app2(p, param[1], param[2])) {
        if (for_all(p, param[0])) {
          _param = param[3];
          continue ;
          
        }
        else {
          return /* false */0;
        }
      }
      else {
        return /* false */0;
      }
    }
    else {
      return /* true */1;
    }
  };
}

function exists(p, _param) {
  while(true) {
    var param = _param;
    if (param) {
      if (Caml_curry.app2(p, param[1], param[2])) {
        return /* true */1;
      }
      else if (exists(p, param[0])) {
        return /* true */1;
      }
      else {
        _param = param[3];
        continue ;
        
      }
    }
    else {
      return /* false */0;
    }
  };
}

function add_min_binding(k, v, param) {
  if (param) {
    return bal(add_min_binding(k, v, param[0]), param[1], param[2], param[3]);
  }
  else {
    return singleton(k, v);
  }
}

function add_max_binding(k, v, param) {
  if (param) {
    return bal(param[0], param[1], param[2], add_max_binding(k, v, param[3]));
  }
  else {
    return singleton(k, v);
  }
}

function join(l, v, d, r) {
  if (l) {
    if (r) {
      var rh = r[4];
      var lh = l[4];
      if (lh > rh + 2) {
        return bal(l[0], l[1], l[2], join(l[3], v, d, r));
      }
      else if (rh > lh + 2) {
        return bal(join(l, v, d, r[0]), r[1], r[2], r[3]);
      }
      else {
        return create(l, v, d, r);
      }
    }
    else {
      return add_max_binding(v, d, l);
    }
  }
  else {
    return add_min_binding(v, d, r);
  }
}

function concat(t1, t2) {
  if (t1) {
    if (t2) {
      var match = min_binding(t2);
      return join(t1, match[0], match[1], remove_min_binding(t2));
    }
    else {
      return t1;
    }
  }
  else {
    return t2;
  }
}

function concat_or_join(t1, v, d, t2) {
  if (d) {
    return join(t1, v, d[0], t2);
  }
  else {
    return concat(t1, t2);
  }
}

function split(x, param) {
  if (param) {
    var r = param[3];
    var d = param[2];
    var v = param[1];
    var l = param[0];
    var c = Caml_obj.caml_int_compare(x, v);
    if (c) {
      if (c < 0) {
        var match = split(x, l);
        return /* tuple */[
                match[0],
                match[1],
                join(match[2], v, d, r)
              ];
      }
      else {
        var match$1 = split(x, r);
        return /* tuple */[
                join(l, v, d, match$1[0]),
                match$1[1],
                match$1[2]
              ];
      }
    }
    else {
      return /* tuple */[
              l,
              /* Some */[d],
              r
            ];
    }
  }
  else {
    return /* tuple */[
            /* Empty */0,
            /* None */0,
            /* Empty */0
          ];
  }
}

function merge(f, s1, s2) {
  var exit = 0;
  if (s1) {
    var v1 = s1[1];
    if (s1[4] >= height(s2)) {
      var match = split(v1, s2);
      return concat_or_join(merge(f, s1[0], match[0]), v1, Caml_curry.app3(f, v1, /* Some */[s1[2]], match[1]), merge(f, s1[3], match[2]));
    }
    else {
      exit = 1;
    }
  }
  else if (s2) {
    exit = 1;
  }
  else {
    return /* Empty */0;
  }
  if (exit === 1) {
    if (s2) {
      var v2 = s2[1];
      var match$1 = split(v2, s1);
      return concat_or_join(merge(f, match$1[0], s2[0]), v2, Caml_curry.app3(f, v2, match$1[1], /* Some */[s2[2]]), merge(f, match$1[2], s2[3]));
    }
    else {
      throw [
            Caml_builtin_exceptions.assert_failure,
            [
              "map.ml",
              270,
              10
            ]
          ];
    }
  }
  
}

function filter(p, param) {
  if (param) {
    var d = param[2];
    var v = param[1];
    var l$prime = filter(p, param[0]);
    var pvd = Caml_curry.app2(p, v, d);
    var r$prime = filter(p, param[3]);
    if (pvd) {
      return join(l$prime, v, d, r$prime);
    }
    else {
      return concat(l$prime, r$prime);
    }
  }
  else {
    return /* Empty */0;
  }
}

function partition(p, param) {
  if (param) {
    var d = param[2];
    var v = param[1];
    var match = partition(p, param[0]);
    var lf = match[1];
    var lt = match[0];
    var pvd = Caml_curry.app2(p, v, d);
    var match$1 = partition(p, param[3]);
    var rf = match$1[1];
    var rt = match$1[0];
    if (pvd) {
      return /* tuple */[
              join(lt, v, d, rt),
              concat(lf, rf)
            ];
    }
    else {
      return /* tuple */[
              concat(lt, rt),
              join(lf, v, d, rf)
            ];
    }
  }
  else {
    return /* tuple */[
            /* Empty */0,
            /* Empty */0
          ];
  }
}

function cons_enum(_m, _e) {
  while(true) {
    var e = _e;
    var m = _m;
    if (m) {
      _e = /* More */{
        0: m[1],
        1: m[2],
        2: m[3],
        3: e,
        length: 4,
        tag: 0
      };
      _m = m[0];
      continue ;
      
    }
    else {
      return e;
    }
  };
}

function compare(cmp, m1, m2) {
  var _e1 = cons_enum(m1, /* End */0);
  var _e2 = cons_enum(m2, /* End */0);
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (e1) {
      if (e2) {
        var c = Caml_obj.caml_int_compare(e1[0], e2[0]);
        if (c !== 0) {
          return c;
        }
        else {
          var c$1 = Caml_curry.app2(cmp, e1[1], e2[1]);
          if (c$1 !== 0) {
            return c$1;
          }
          else {
            _e2 = cons_enum(e2[2], e2[3]);
            _e1 = cons_enum(e1[2], e1[3]);
            continue ;
            
          }
        }
      }
      else {
        return 1;
      }
    }
    else if (e2) {
      return -1;
    }
    else {
      return 0;
    }
  };
}

function equal(cmp, m1, m2) {
  var _e1 = cons_enum(m1, /* End */0);
  var _e2 = cons_enum(m2, /* End */0);
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (e1) {
      if (e2) {
        if (e1[0] === e2[0]) {
          if (Caml_curry.app2(cmp, e1[1], e2[1])) {
            _e2 = cons_enum(e2[2], e2[3]);
            _e1 = cons_enum(e1[2], e1[3]);
            continue ;
            
          }
          else {
            return /* false */0;
          }
        }
        else {
          return /* false */0;
        }
      }
      else {
        return /* false */0;
      }
    }
    else if (e2) {
      return /* false */0;
    }
    else {
      return /* true */1;
    }
  };
}

function cardinal(param) {
  if (param) {
    return cardinal(param[0]) + 1 + cardinal(param[3]);
  }
  else {
    return 0;
  }
}

function bindings_aux(_accu, _param) {
  while(true) {
    var param = _param;
    var accu = _accu;
    if (param) {
      _param = param[0];
      _accu = /* :: */[
        /* tuple */[
          param[1],
          param[2]
        ],
        bindings_aux(accu, param[3])
      ];
      continue ;
      
    }
    else {
      return accu;
    }
  };
}

function bindings(s) {
  return bindings_aux(/* [] */0, s);
}

var IntMap = [
  /* Empty */0,
  is_empty,
  mem,
  add,
  singleton,
  remove,
  merge,
  compare,
  equal,
  iter,
  fold,
  for_all,
  exists,
  filter,
  partition,
  cardinal,
  bindings,
  min_binding,
  max_binding,
  min_binding,
  split,
  find,
  map,
  mapi
];

function assertion_test() {
  var m = /* Empty */0;
  var count = 1000000;
  for(var i = 0; i<= count; ++i){
    m = add(i, i, m);
  }
  for(var i$1 = 0; i$1<= count; ++i$1){
    find(i$1, m);
  }
  return /* () */0;
}

exports.IntMap         = IntMap;
exports.assertion_test = assertion_test;
/* No side effect */
