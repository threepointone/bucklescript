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

(* Author: Rahul Kumar *)
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident


(* Define types of indications supported by the FFI *)
type indication = 
  | Indication_read
  | Indication_write
  | Indication_unsafe

(* Define a comparator to sort indications with *)
let compare_indication : indication -> indication -> int = compare

(* Given an attribute string, determine the indications *)
let classify_indication name = 
  match name with
  | "r" -> [Indication_read]
  | "w" -> [Indication_write]
  | "rw" -> [Indication_read;Indication_write]
  | "unsafe" -> [Indication_unsafe]
  | _ -> []

(* Get unique indications from an attribute list *)
let indications_of_attr attrlst = 
  List.sort_uniq compare_indication (List.concat
                                       (List.map (fun ({txt=x;_},_) -> classify_indication x) attrlst))

(* Define the methods generated by this ppx extension *)
type methods = 
  | Method_read
  | Method_read_unsafe
  | Method_write
  | Method_arity
  | Method_arity_unsafe

(* Define the formatter for printing errors *)
let fmtr = Format.err_formatter


(* Predicated print warning to stderr *)
let pwarn loc predicate message = 
  if predicate then 
    begin
      let open Location in
      let () = Printf.fprintf stderr "\n%!" in
      let () = print_loc fmtr loc in
      Format.fprintf fmtr ": %s\n%!" message
    end
  else 
    ()

(* Define which methods get generated based on indications *)
let methods_of_indications i loc = 
  match i with
  | [] -> [Method_arity]
  | [Indication_read] -> [Method_read]
  | [Indication_read;
     Indication_unsafe] -> [Method_read_unsafe]
  | [Indication_read;
     Indication_write] -> [Method_read;Method_write]
  | [Indication_read;
     Indication_write;
     Indication_unsafe] -> [Method_read_unsafe;Method_write]
  | [Indication_write] -> [Method_write]
  | [Indication_unsafe] -> [Method_arity_unsafe]
  | _ -> begin
      pwarn loc true "Invalid combination of indications."; 
      []
    end

(* Filter out the attributes expanded by this ppx from an attribute list *)
let filter_attributes attrlst = List.filter 
    (fun ({txt=x;_},_) -> classify_indication x = []) attrlst

(* Filter out the attributes expanded by this ppx from a core_type*)
let strip_attr t = {t with ptyp_attributes=filter_attributes t.ptyp_attributes}

(* Count the arity of a core_type *)
let rec count_arity z = 
  match z with
  | {ptyp_desc = Ptyp_arrow (_,_,y);_} -> 1 + (count_arity y)
  | _ -> 0

(* Determine whether a core_type is a non-nullary function *)
let is_non_nullary_fn z = (count_arity z) > 0

(* Produce a new core_type from the given core_type, removing 'opt/option/def'
   from the return type*)
let rec optless typ loc prefix = 
  match typ with
  | {ptyp_desc = 
       Ptyp_constr ({txt=
                       (Lident ("opt"|"def"|"opt_def")) | 
                       (Ldot (Lident "Js",
                              ("opt"|"def"|"opt_def")));_},[x]);_} -> x
  | {ptyp_desc = Ptyp_arrow  (tag,t1,t2);_} as t_ -> 
    {t_ with ptyp_desc = Ptyp_arrow (tag,t1,optless t2 loc prefix)}
  | _ -> begin
      pwarn loc true (prefix^"indication provided for non-option type."); 
      typ
    end

(* Generate method signature with appropriate warnings/checks *)
let gen_method loc original abs vis name attr typ meth = 
  match meth with
  | Method_read -> begin
      let () = pwarn loc (is_non_nullary_fn typ) 
          "Read indication provided for non-property."
      in
      let t = strip_attr typ in
      {original with pctf_desc = Pctf_method (name^"__r",abs,vis,t)}
    end
  | Method_read_unsafe -> begin
      let () = pwarn loc (is_non_nullary_fn typ) 
          "Read (unsafe) indication provided for non-property."
      in
      let t1 = optless typ loc "Read (unsafe) " in
      let t = strip_attr t1 in
      {original with pctf_desc = Pctf_method (name^"__r_unsafe",abs,vis,t)}
    end
  | Method_write -> begin
      let () = pwarn loc (is_non_nullary_fn typ)
          "Write indication provided for non-property."
      in
      let t = strip_attr typ in
      {original with pctf_desc = Pctf_method (name^"__w",abs,vis,t)}
    end
  | Method_arity -> begin
      let t = strip_attr typ in
      {original with pctf_desc = Pctf_method (
           name^"__"^(string_of_int (count_arity typ)),abs,vis,t)}
    end
  | Method_arity_unsafe -> begin
      let t1 = optless typ loc "Unsafe method " in
      let t = strip_attr t1 in
      {original with pctf_desc = Pctf_method (
           name^"__"^(string_of_int (count_arity typ))^"_unsafe",abs,vis,t)}
    end

(* Check that a string contains '__', without using the Str library *)
let contains_undscr x = 
  let cnt = ref 0 in
  let l = String.length x in
  let () = String.iteri (fun i c ->
      if c='_' then
        begin
          if (i < l-1) && (String.get x (i+1) = '_') then
            cnt := !cnt + 1
          else
            ()
        end
      else
        ()) x in
  (!cnt > 0)

(* Generate the FFI method signature given original method declaration *)
let new_methods m = 
  match m with 
  | {pctf_desc = Pctf_method (name,_,_,_);_} as c 
    when contains_undscr name -> [c]
  | {pctf_desc = Pctf_method (name,x,y,z);_} as c ->
    begin
      let loc = z.ptyp_loc in
      let attr = z.ptyp_attributes in
      let i = indications_of_attr attr in
      List.map (gen_method loc c x y name attr z) 
        (methods_of_indications i loc)
    end
  | _ -> [m]

(* Map class_type_declaration for FFI *)
let new_ctd mapper ctd = 
  match ctd with
  | { pci_name=name; 
      pci_expr = 
        { pcty_desc = Pcty_signature (
             { pcsig_fields = methodlst;_ } as flds);
          _} as expr; 
      pci_attributes = attrlst
    } as c -> 
    { c with pci_expr = 
               { expr with pcty_desc = Pcty_signature 
                               { flds with pcsig_fields = 
                                             (List.concat (List.map new_methods methodlst)) } };
    }
  | _ -> default_mapper.class_type_declaration mapper ctd

let ctd_mapper argv = {
  default_mapper with
  class_type_declaration = (fun mapper ctd -> new_ctd mapper ctd);
}

let () = register "ppx_class" ctd_mapper
