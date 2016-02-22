module BA1 = Bigarray.Array1

let v = BA1.create Int32 C_layout 20;;

let sum (v : (int32, 'a, 'b) BA1.t )  = 
  let result = ref 0l  in
  for i = 0 to BA1.dim v - 1 do (* caml_ba_dim_1 *)
    result := Int32.add (!result)  v.{i} (* caml_ba_get_1*)
  done   

let vv = BA1.create Int32 Fortran_layout 30 ;;

let init : (int32, 'a, 'b) BA1.t -> unit  = fun  v -> 
  for i = 0 to BA1.dim v - 1 do (* caml_ba_dim_1 *)
    let i = Int32.of_int i in
    v.{Int32.to_int i} <- Int32.mul i  i (* caml_ba_set_1 *)
  done


let a = 
  init v ; 
  (sum v(* , sum vv *))  
