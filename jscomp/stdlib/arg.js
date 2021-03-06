// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("../runtime/caml_builtin_exceptions");
var Caml_obj                = require("../runtime/caml_obj");
var Pervasives              = require("./pervasives");
var Caml_format             = require("../runtime/caml_format");
var Sys                     = require("./sys");
var Printf                  = require("./printf");
var Buffer                  = require("./buffer");
var Caml_curry              = require("../runtime/caml_curry");
var $$String                = require("./string");
var List                    = require("./list");

var Bad = {
  0: "Arg.Bad",
  1: Caml_builtin_exceptions.get_id(),
  length: 2,
  tag: 248
};

var Help = {
  0: "Arg.Help",
  1: Caml_builtin_exceptions.get_id(),
  length: 2,
  tag: 248
};

var Stop = {
  0: "Arg.Stop",
  1: Caml_builtin_exceptions.get_id(),
  length: 2,
  tag: 248
};

function assoc3(x, _l) {
  while(true) {
    var l = _l;
    if (l) {
      var match = l[0];
      if (Caml_obj.caml_equal(match[0], x)) {
        return match[1];
      }
      else {
        _l = l[1];
        continue ;
        
      }
    }
    else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
}

function make_symlist(prefix, sep, suffix, l) {
  if (l) {
    return List.fold_left(function (x, y) {
                return x + (sep + y);
              }, prefix + l[0], l[1]) + suffix;
  }
  else {
    return "<none>";
  }
}

function help_action() {
  throw [
        Stop,
        /* Unknown */{
          0: "-help",
          length: 1,
          tag: 0
        }
      ];
}

function add_help(speclist) {
  var add1;
  try {
    assoc3("-help", speclist);
    add1 = /* [] */0;
  }
  catch (exn){
    if (exn === Caml_builtin_exceptions.not_found) {
      add1 = /* :: */[
        /* tuple */[
          "-help",
          /* Unit */{
            0: help_action,
            length: 1,
            tag: 0
          },
          " Display this list of options"
        ],
        /* [] */0
      ];
    }
    else {
      throw exn;
    }
  }
  var add2;
  try {
    assoc3("--help", speclist);
    add2 = /* [] */0;
  }
  catch (exn$1){
    if (exn$1 === Caml_builtin_exceptions.not_found) {
      add2 = /* :: */[
        /* tuple */[
          "--help",
          /* Unit */{
            0: help_action,
            length: 1,
            tag: 0
          },
          " Display this list of options"
        ],
        /* [] */0
      ];
    }
    else {
      throw exn$1;
    }
  }
  return Pervasives.$at(speclist, Pervasives.$at(add1, add2));
}

function usage_b(buf, speclist, errmsg) {
  Caml_curry.app1(Printf.bprintf(buf, /* Format */{
            0: /* String */{
              0: /* No_padding */0,
              1: /* Char_literal */{
                0: /* "\n" */10,
                1: /* End_of_format */0,
                length: 2,
                tag: 12
              },
              length: 2,
              tag: 2
            },
            1: "%s\n",
            length: 2,
            tag: 0
          }), errmsg);
  return List.iter(function (param) {
              var buf$1 = buf;
              var param$1 = param;
              var doc = param$1[2];
              var spec = param$1[1];
              var key = param$1[0];
              if (doc.length) {
                if (spec.tag === 11) {
                  return Caml_curry.app3(Printf.bprintf(buf$1, /* Format */{
                                  0: /* String_literal */{
                                    0: "  ",
                                    1: /* String */{
                                      0: /* No_padding */0,
                                      1: /* Char_literal */{
                                        0: /* " " */32,
                                        1: /* String */{
                                          0: /* No_padding */0,
                                          1: /* String */{
                                            0: /* No_padding */0,
                                            1: /* Char_literal */{
                                              0: /* "\n" */10,
                                              1: /* End_of_format */0,
                                              length: 2,
                                              tag: 12
                                            },
                                            length: 2,
                                            tag: 2
                                          },
                                          length: 2,
                                          tag: 2
                                        },
                                        length: 2,
                                        tag: 12
                                      },
                                      length: 2,
                                      tag: 2
                                    },
                                    length: 2,
                                    tag: 11
                                  },
                                  1: "  %s %s%s\n",
                                  length: 2,
                                  tag: 0
                                }), key, make_symlist("{", "|", "}", spec[0]), doc);
                }
                else {
                  return Caml_curry.app2(Printf.bprintf(buf$1, /* Format */{
                                  0: /* String_literal */{
                                    0: "  ",
                                    1: /* String */{
                                      0: /* No_padding */0,
                                      1: /* Char_literal */{
                                        0: /* " " */32,
                                        1: /* String */{
                                          0: /* No_padding */0,
                                          1: /* Char_literal */{
                                            0: /* "\n" */10,
                                            1: /* End_of_format */0,
                                            length: 2,
                                            tag: 12
                                          },
                                          length: 2,
                                          tag: 2
                                        },
                                        length: 2,
                                        tag: 12
                                      },
                                      length: 2,
                                      tag: 2
                                    },
                                    length: 2,
                                    tag: 11
                                  },
                                  1: "  %s %s\n",
                                  length: 2,
                                  tag: 0
                                }), key, doc);
                }
              }
              else {
                return 0;
              }
            }, add_help(speclist));
}

function usage_string(speclist, errmsg) {
  var b = Buffer.create(200);
  usage_b(b, speclist, errmsg);
  return Buffer.contents(b);
}

function usage(speclist, errmsg) {
  return Caml_curry.app1(Printf.eprintf(/* Format */{
                  0: /* String */{
                    0: /* No_padding */0,
                    1: /* End_of_format */0,
                    length: 2,
                    tag: 2
                  },
                  1: "%s",
                  length: 2,
                  tag: 0
                }), usage_string(speclist, errmsg));
}

var current = [0];

function parse_argv_dynamic($staropt$star, argv, speclist, anonfun, errmsg) {
  var current$1 = $staropt$star ? $staropt$star[0] : current;
  var l = argv.length;
  var b = Buffer.create(200);
  var initpos = current$1[0];
  var stop = function (error) {
    var progname = initpos < l ? argv[initpos] : "(?)";
    switch (error.tag | 0) {
      case 0 : 
          var s = error[0];
          switch (s) {
            case "--help" : 
            case "-help" : 
                break;
            default:
              Caml_curry.app2(Printf.bprintf(b, /* Format */{
                        0: /* String */{
                          0: /* No_padding */0,
                          1: /* String_literal */{
                            0: ": unknown option '",
                            1: /* String */{
                              0: /* No_padding */0,
                              1: /* String_literal */{
                                0: "'.\n",
                                1: /* End_of_format */0,
                                length: 2,
                                tag: 11
                              },
                              length: 2,
                              tag: 2
                            },
                            length: 2,
                            tag: 11
                          },
                          length: 2,
                          tag: 2
                        },
                        1: "%s: unknown option '%s'.\n",
                        length: 2,
                        tag: 0
                      }), progname, s);
          }
          break;
      case 1 : 
          Caml_curry.app4(Printf.bprintf(b, /* Format */{
                    0: /* String */{
                      0: /* No_padding */0,
                      1: /* String_literal */{
                        0: ": wrong argument '",
                        1: /* String */{
                          0: /* No_padding */0,
                          1: /* String_literal */{
                            0: "'; option '",
                            1: /* String */{
                              0: /* No_padding */0,
                              1: /* String_literal */{
                                0: "' expects ",
                                1: /* String */{
                                  0: /* No_padding */0,
                                  1: /* String_literal */{
                                    0: ".\n",
                                    1: /* End_of_format */0,
                                    length: 2,
                                    tag: 11
                                  },
                                  length: 2,
                                  tag: 2
                                },
                                length: 2,
                                tag: 11
                              },
                              length: 2,
                              tag: 2
                            },
                            length: 2,
                            tag: 11
                          },
                          length: 2,
                          tag: 2
                        },
                        length: 2,
                        tag: 11
                      },
                      length: 2,
                      tag: 2
                    },
                    1: "%s: wrong argument '%s'; option '%s' expects %s.\n",
                    length: 2,
                    tag: 0
                  }), progname, error[1], error[0], error[2]);
          break;
      case 2 : 
          Caml_curry.app2(Printf.bprintf(b, /* Format */{
                    0: /* String */{
                      0: /* No_padding */0,
                      1: /* String_literal */{
                        0: ": option '",
                        1: /* String */{
                          0: /* No_padding */0,
                          1: /* String_literal */{
                            0: "' needs an argument.\n",
                            1: /* End_of_format */0,
                            length: 2,
                            tag: 11
                          },
                          length: 2,
                          tag: 2
                        },
                        length: 2,
                        tag: 11
                      },
                      length: 2,
                      tag: 2
                    },
                    1: "%s: option '%s' needs an argument.\n",
                    length: 2,
                    tag: 0
                  }), progname, error[0]);
          break;
      case 3 : 
          Caml_curry.app2(Printf.bprintf(b, /* Format */{
                    0: /* String */{
                      0: /* No_padding */0,
                      1: /* String_literal */{
                        0: ": ",
                        1: /* String */{
                          0: /* No_padding */0,
                          1: /* String_literal */{
                            0: ".\n",
                            1: /* End_of_format */0,
                            length: 2,
                            tag: 11
                          },
                          length: 2,
                          tag: 2
                        },
                        length: 2,
                        tag: 11
                      },
                      length: 2,
                      tag: 2
                    },
                    1: "%s: %s.\n",
                    length: 2,
                    tag: 0
                  }), progname, error[0]);
          break;
      
    }
    usage_b(b, speclist[0], errmsg);
    if (Caml_obj.caml_equal(error, /* Unknown */{
            0: "-help",
            length: 1,
            tag: 0
          }) || Caml_obj.caml_equal(error, /* Unknown */{
            0: "--help",
            length: 1,
            tag: 0
          })) {
      throw [
            Help,
            Buffer.contents(b)
          ];
    }
    else {
      throw [
            Bad,
            Buffer.contents(b)
          ];
    }
  };
  ++ current$1[0];
  while(current$1[0] < l) {
    var s = argv[current$1[0]];
    if (s.length >= 1 && s[0] === "-") {
      var action;
      try {
        action = assoc3(s, speclist[0]);
      }
      catch (exn){
        if (exn === Caml_builtin_exceptions.not_found) {
          action = stop(/* Unknown */{
                0: s,
                length: 1,
                tag: 0
              });
        }
        else {
          throw exn;
        }
      }
      try {
        var treat_action = (function(s){
        return function (param) {
          switch (param.tag | 0) {
            case 0 : 
                return Caml_curry.app1(param[0], /* () */0);
            case 1 : 
                if (current$1[0] + 1 < l) {
                  var arg = argv[current$1[0] + 1];
                  try {
                    Caml_curry.app1(param[0], Pervasives.bool_of_string(arg));
                  }
                  catch (exn){
                    if (exn[0] === Caml_builtin_exceptions.invalid_argument) {
                      if (exn[1] === "bool_of_string") {
                        throw [
                              Stop,
                              /* Wrong */{
                                0: s,
                                1: arg,
                                2: "a boolean",
                                length: 3,
                                tag: 1
                              }
                            ];
                      }
                      else {
                        throw exn;
                      }
                    }
                    else {
                      throw exn;
                    }
                  }
                  return ++ current$1[0];
                }
                else {
                  throw [
                        Stop,
                        /* Missing */{
                          0: s,
                          length: 1,
                          tag: 2
                        }
                      ];
                }
                break;
            case 2 : 
                param[0][0] = /* true */1;
                return /* () */0;
            case 3 : 
                param[0][0] = /* false */0;
                return /* () */0;
            case 4 : 
                if (current$1[0] + 1 < l) {
                  Caml_curry.app1(param[0], argv[current$1[0] + 1]);
                  return ++ current$1[0];
                }
                else {
                  throw [
                        Stop,
                        /* Missing */{
                          0: s,
                          length: 1,
                          tag: 2
                        }
                      ];
                }
                break;
            case 5 : 
                if (current$1[0] + 1 < l) {
                  param[0][0] = argv[current$1[0] + 1];
                  return ++ current$1[0];
                }
                else {
                  throw [
                        Stop,
                        /* Missing */{
                          0: s,
                          length: 1,
                          tag: 2
                        }
                      ];
                }
                break;
            case 6 : 
                if (current$1[0] + 1 < l) {
                  var arg$1 = argv[current$1[0] + 1];
                  try {
                    Caml_curry.app1(param[0], Caml_format.caml_int_of_string(arg$1));
                  }
                  catch (exn$1){
                    if (exn$1[0] === Caml_builtin_exceptions.failure) {
                      if (exn$1[1] === "int_of_string") {
                        throw [
                              Stop,
                              /* Wrong */{
                                0: s,
                                1: arg$1,
                                2: "an integer",
                                length: 3,
                                tag: 1
                              }
                            ];
                      }
                      else {
                        throw exn$1;
                      }
                    }
                    else {
                      throw exn$1;
                    }
                  }
                  return ++ current$1[0];
                }
                else {
                  throw [
                        Stop,
                        /* Missing */{
                          0: s,
                          length: 1,
                          tag: 2
                        }
                      ];
                }
                break;
            case 7 : 
                if (current$1[0] + 1 < l) {
                  var arg$2 = argv[current$1[0] + 1];
                  try {
                    param[0][0] = Caml_format.caml_int_of_string(arg$2);
                  }
                  catch (exn$2){
                    if (exn$2[0] === Caml_builtin_exceptions.failure) {
                      if (exn$2[1] === "int_of_string") {
                        throw [
                              Stop,
                              /* Wrong */{
                                0: s,
                                1: arg$2,
                                2: "an integer",
                                length: 3,
                                tag: 1
                              }
                            ];
                      }
                      else {
                        throw exn$2;
                      }
                    }
                    else {
                      throw exn$2;
                    }
                  }
                  return ++ current$1[0];
                }
                else {
                  throw [
                        Stop,
                        /* Missing */{
                          0: s,
                          length: 1,
                          tag: 2
                        }
                      ];
                }
                break;
            case 8 : 
                if (current$1[0] + 1 < l) {
                  var arg$3 = argv[current$1[0] + 1];
                  try {
                    Caml_curry.app1(param[0], Caml_format.caml_float_of_string(arg$3));
                  }
                  catch (exn$3){
                    if (exn$3[0] === Caml_builtin_exceptions.failure) {
                      if (exn$3[1] === "float_of_string") {
                        throw [
                              Stop,
                              /* Wrong */{
                                0: s,
                                1: arg$3,
                                2: "a float",
                                length: 3,
                                tag: 1
                              }
                            ];
                      }
                      else {
                        throw exn$3;
                      }
                    }
                    else {
                      throw exn$3;
                    }
                  }
                  return ++ current$1[0];
                }
                else {
                  throw [
                        Stop,
                        /* Missing */{
                          0: s,
                          length: 1,
                          tag: 2
                        }
                      ];
                }
                break;
            case 9 : 
                if (current$1[0] + 1 < l) {
                  var arg$4 = argv[current$1[0] + 1];
                  try {
                    param[0][0] = Caml_format.caml_float_of_string(arg$4);
                  }
                  catch (exn$4){
                    if (exn$4[0] === Caml_builtin_exceptions.failure) {
                      if (exn$4[1] === "float_of_string") {
                        throw [
                              Stop,
                              /* Wrong */{
                                0: s,
                                1: arg$4,
                                2: "a float",
                                length: 3,
                                tag: 1
                              }
                            ];
                      }
                      else {
                        throw exn$4;
                      }
                    }
                    else {
                      throw exn$4;
                    }
                  }
                  return ++ current$1[0];
                }
                else {
                  throw [
                        Stop,
                        /* Missing */{
                          0: s,
                          length: 1,
                          tag: 2
                        }
                      ];
                }
                break;
            case 10 : 
                return List.iter(treat_action, param[0]);
            case 11 : 
                var symb = param[0];
                if (current$1[0] + 1 < l) {
                  var arg$5 = argv[current$1[0] + 1];
                  if (List.mem(arg$5, symb)) {
                    Caml_curry.app1(param[1], argv[current$1[0] + 1]);
                    return ++ current$1[0];
                  }
                  else {
                    throw [
                          Stop,
                          /* Wrong */{
                            0: s,
                            1: arg$5,
                            2: "one of: " + make_symlist("", " ", "", symb),
                            length: 3,
                            tag: 1
                          }
                        ];
                  }
                }
                else {
                  throw [
                        Stop,
                        /* Missing */{
                          0: s,
                          length: 1,
                          tag: 2
                        }
                      ];
                }
                break;
            case 12 : 
                var f = param[0];
                while(current$1[0] < l - 1) {
                  Caml_curry.app1(f, argv[current$1[0] + 1]);
                  ++ current$1[0];
                };
                return /* () */0;
            
          }
        }
        }(s));
        treat_action(action);
      }
      catch (exn$1){
        if (exn$1[0] === Bad) {
          stop(/* Message */{
                0: exn$1[1],
                length: 1,
                tag: 3
              });
        }
        else if (exn$1[0] === Stop) {
          stop(exn$1[1]);
        }
        else {
          throw exn$1;
        }
      }
      ++ current$1[0];
    }
    else {
      try {
        Caml_curry.app1(anonfun, s);
      }
      catch (exn$2){
        if (exn$2[0] === Bad) {
          stop(/* Message */{
                0: exn$2[1],
                length: 1,
                tag: 3
              });
        }
        else {
          throw exn$2;
        }
      }
      ++ current$1[0];
    }
  };
  return /* () */0;
}

function parse_argv($staropt$star, argv, speclist, anonfun, errmsg) {
  var current$1 = $staropt$star ? $staropt$star[0] : current;
  return parse_argv_dynamic(/* Some */[current$1], argv, [speclist], anonfun, errmsg);
}

function parse(l, f, msg) {
  try {
    return parse_argv(/* None */0, Sys.argv, l, f, msg);
  }
  catch (exn){
    if (exn[0] === Bad) {
      Caml_curry.app1(Printf.eprintf(/* Format */{
                0: /* String */{
                  0: /* No_padding */0,
                  1: /* End_of_format */0,
                  length: 2,
                  tag: 2
                },
                1: "%s",
                length: 2,
                tag: 0
              }), exn[1]);
      return Pervasives.exit(2);
    }
    else if (exn[0] === Help) {
      Caml_curry.app1(Printf.printf(/* Format */{
                0: /* String */{
                  0: /* No_padding */0,
                  1: /* End_of_format */0,
                  length: 2,
                  tag: 2
                },
                1: "%s",
                length: 2,
                tag: 0
              }), exn[1]);
      return Pervasives.exit(0);
    }
    else {
      throw exn;
    }
  }
}

function parse_dynamic(l, f, msg) {
  try {
    return parse_argv_dynamic(/* None */0, Sys.argv, l, f, msg);
  }
  catch (exn){
    if (exn[0] === Bad) {
      Caml_curry.app1(Printf.eprintf(/* Format */{
                0: /* String */{
                  0: /* No_padding */0,
                  1: /* End_of_format */0,
                  length: 2,
                  tag: 2
                },
                1: "%s",
                length: 2,
                tag: 0
              }), exn[1]);
      return Pervasives.exit(2);
    }
    else if (exn[0] === Help) {
      Caml_curry.app1(Printf.printf(/* Format */{
                0: /* String */{
                  0: /* No_padding */0,
                  1: /* End_of_format */0,
                  length: 2,
                  tag: 2
                },
                1: "%s",
                length: 2,
                tag: 0
              }), exn[1]);
      return Pervasives.exit(0);
    }
    else {
      throw exn;
    }
  }
}

function second_word(s) {
  var len = s.length;
  try {
    var _n = $$String.index(s, /* " " */32);
    while(true) {
      var n = _n;
      if (n >= len) {
        return len;
      }
      else if (s[n] === " ") {
        _n = n + 1;
        continue ;
        
      }
      else {
        return n;
      }
    };
  }
  catch (exn){
    if (exn === Caml_builtin_exceptions.not_found) {
      return len;
    }
    else {
      throw exn;
    }
  }
}

function max_arg_len(cur, param) {
  var kwd = param[0];
  if (param[1].tag === 11) {
    return Pervasives.max(cur, kwd.length);
  }
  else {
    return Pervasives.max(cur, kwd.length + second_word(param[2]));
  }
}

function align($staropt$star, speclist) {
  var limit = $staropt$star ? $staropt$star[0] : Pervasives.max_int;
  var completed = add_help(speclist);
  var len = List.fold_left(max_arg_len, 0, completed);
  var len$1 = Pervasives.min(len, limit);
  return List.map(function (param) {
              var len$2 = len$1;
              var ksd = param;
              var spec = ksd[1];
              var kwd = ksd[0];
              if (ksd[2] === "") {
                return ksd;
              }
              else if (spec.tag === 11) {
                var msg = ksd[2];
                var cutcol = second_word(msg);
                var spaces = $$String.make(Pervasives.max(0, len$2 - cutcol) + 3, /* " " */32);
                return /* tuple */[
                        kwd,
                        spec,
                        "\n" + (spaces + msg)
                      ];
              }
              else {
                var msg$1 = ksd[2];
                var cutcol$1 = second_word(msg$1);
                var kwd_len = kwd.length;
                var diff = len$2 - kwd_len - cutcol$1;
                if (diff <= 0) {
                  return /* tuple */[
                          kwd,
                          spec,
                          msg$1
                        ];
                }
                else {
                  var spaces$1 = $$String.make(diff, /* " " */32);
                  var prefix = $$String.sub(msg$1, 0, cutcol$1);
                  var suffix = $$String.sub(msg$1, cutcol$1, msg$1.length - cutcol$1);
                  return /* tuple */[
                          kwd,
                          spec,
                          prefix + (spaces$1 + suffix)
                        ];
                }
              }
            }, completed);
}

exports.parse              = parse;
exports.parse_dynamic      = parse_dynamic;
exports.parse_argv         = parse_argv;
exports.parse_argv_dynamic = parse_argv_dynamic;
exports.Help               = Help;
exports.Bad                = Bad;
exports.usage              = usage;
exports.usage_string       = usage_string;
exports.align              = align;
exports.current            = current;
/* No side effect */
