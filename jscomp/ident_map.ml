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



(** Map with key specialized as [Ident] type, enhanced with some utilities *)

include Map.Make(struct 
  type t = Ident.t 
  let compare  = Pervasives.compare (**TODO: fix me*)
end)

let of_list lst = 
  List.fold_left (fun acc (k,v) -> add k v acc) empty lst

let keys map = fold (fun k _ acc -> k::acc  ) map []

(* TODO: have this in stdlib/map to save some time *)
let add_if_not_exist key v m = 
  if mem key m then m else add key v m

let merge_disjoint m1 m2 = 
  merge 
    (fun k x0 y0 -> 
       match x0, y0 with 
         None, None -> None
       | None, Some v | Some v, None -> Some v
       | _, _ -> invalid_arg "merge_disjoint: maps are not disjoint")
    m1 m2
