type expr =
  | A of int*expr
  | P of int*expr
  | BF of int*expr
  | TF of expr*int
  | R of int
;;


let remove_sq_from_denom e =
  match e with
  | BF(n, A(p, R(k))) ->
     let denom = k - p*p in
     if denom mod n = 0
     then TF(A(-p, R(k)), (denom/n))
     else failwith "ne se simplifie pas bien"       
     
  | _ -> failwith "pas possible"
;;

let a = BF(1, A(-4, R(23)));;
let a' = remove_sq_from_denom a;;

let rec evaluate e =
  match e with
  | A(n,e') -> float_of_int n +. (evaluate e')
  | P(n,e') -> float_of_int n *. (evaluate e')
  | BF(n, e') -> float_of_int n /. (evaluate e')
  | TF(e',n) -> (evaluate e') /. float_of_int n
  | R(n) -> sqrt(float_of_int n)
;;
     
let separate_int e =
  match e with
  | TF(A(p, R(k)),q) ->
     let n = int_of_float (evaluate e) in
     (n, TF(A(p-n*q, R(k)),q))
  | R(k) ->
     let n = int_of_float (evaluate e) in
     (n, TF(A(-n, R(k)),1))
  | _ -> failwith "pas la bonne forme"
;;

let a'' = separate_int a';;

let expand e =
  match e with 
  | TF(A(p, R(k)),q) ->
     separate_int (remove_sq_from_denom (BF(q, A(p, R(k)))))
  | _ -> failwith "pas la bonne forme"
;;
      
let calculate_continued_fraction_sq n =
  
