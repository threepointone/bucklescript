(* BuckleScript compiler
 * Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* Author: Hongbo Zhang  *)



(* TODO: add a magic number *)
type cmj_value = {
  arity : Lam_stats.function_arities ;
  closed_lambda : Lambda.lambda option ; 
  (** Either constant or closed functor *)
}

type effect = string option

type cmj_table = {
  values : cmj_value String_map.t;
  effect : effect;
  goog_package : string option;
}

let cmj_magic_number =  "BUCKLE20160310"
let cmj_magic_number_length = 
  String.length cmj_magic_number

let pure_dummy = 
  {
    values = String_map.empty;
    effect = None;
    goog_package = None
  }

let no_pure_dummy = 
  {
    values = String_map.empty;
    effect = (Some "");
    goog_package = None 
  }



let from_file name : cmj_table =
  let ic = open_in_bin name in 
  let buffer = really_input_string ic cmj_magic_number_length in 
  if buffer <> cmj_magic_number then
    failwith 
      ("cmj files have incompatible versions, please rebuilt using the new compiler : " 
       ^ __LOC__)
  else 
    (input_value ic  : cmj_table)


let from_string s : cmj_table = 
  let magic_number = String.sub s 0 cmj_magic_number_length in 
  if magic_number = cmj_magic_number then 
    Marshal.from_string s  cmj_magic_number_length
  else 
    failwith 
      ("cmj files have incompatible versions, please rebuilt using the new compiler : "
       ^ __LOC__)

let to_file name (v : cmj_table) = 
  let oc = open_out_bin name in 
  output_string oc cmj_magic_number;
  output_value oc v;
  close_out oc 
