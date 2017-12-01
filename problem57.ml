(* #load "nums.cma";;
à taper dans l'interpréteur *)

open Big_int;;
let one = unit_big_int;;
let two = succ_big_int one;;
  
type exp =
  | A of Big_int.big_int*exp
  | FS of Big_int.big_int*Big_int.big_int
  | FC of Big_int.big_int*exp;;
  
let rec simplify e =
  match e with
  | A(n, e') ->
     begin
       match simplify e' with
       | FS(p,q) -> FS(add_big_int (mult_big_int n q)  p, q)
       | _ -> failwith "pas simplifié, A"
     end
  | FC(p, e') -> 
     begin
       match simplify e' with
       | FS(p',q') -> FS(mult_big_int p q',p')
       | _ -> failwith "pas simplifié, FC"
     end
  | FS(_) -> e
;;
                  

let gen_expansion n =
  let rec aux n = 
    match n with
    | 0 -> FS(one,two)
    | _ -> FC(one,A(two,aux (n-1)))
  in
  A(one, aux n)
;;
  
let print_frac p q =
  print_string ((string_of_big_int p)^(",")^(string_of_big_int q)^"\n");;
                  
let results () = 
  let res = ref 0 in  
  for i = 0 to 999 do
    match simplify (gen_expansion i) with
    | FS(p,q) ->
       if String.length (string_of_big_int p) > String.length (string_of_big_int q)
       then (incr res; print_frac p q;)
       
    | _  -> failwith "pas simplifié"
                     
  done;
  !res;;
  
