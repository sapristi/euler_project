

let power k n =
  let rec aux k n res =
    if n = 0
    then res
    else aux k (n-1) (res*k)
  in
  aux k n 1
;;

  
let num_len = ref 1;;
  
let add_ret n ret =
  let total = n + ret in
  let ret = total/(power 10 !num_len) in
  let cropped = total - ret * (power 10 !num_len) in
  (cropped, ret);;
  
let mult_1l k nl = 
  let rec mult_aux k nl retenue res = 
    match nl with
    | [] ->
       if retenue = 0
       then res
       else retenue :: res
    | h :: t ->
       let cropped, ret = add_ret (k*h) retenue in
       mult_aux k t ret (cropped :: res) in
  List.rev (mult_aux k nl 0 []);;

let add_2l l1 l2 = 
  let rec add_2l_aux l1 l2 retenue res = 
    match l1, l2 with
    | [],[] -> if retenue = 0 then res else retenue::res
                                                       
    | h1::t1, [] ->
       let cropped, ret = add_ret h1 retenue in
       add_2l_aux t1 [] ret (cropped :: res)
                  
    | [], h2 :: t2 -> 
       let cropped, ret = add_ret h2 retenue in
       add_2l_aux [] t2 ret (cropped :: res)
                  
    | h1 :: t1, h2 :: t2 -> 
       let cropped, ret = add_ret (h1 + h2) retenue in
       add_2l_aux t1 t2 ret (cropped :: res)
  in
  List.rev (add_2l_aux l1 l2 0 []);;


  
let mult_2l nl1 nl2 =
  let rec mult_2l_aux nl1 nl2 retenue res =
  match nl1 with
  | h :: t ->
     
     let (item ::  ret) = add_2l retenue (mult_1l h nl2) in
     mult_2l_aux t nl2 ret (item::res)

  |[] -> (List.rev retenue) @ res
  in
  List.rev (mult_2l_aux nl1 nl2 [] [])
;;
     
let deux10 = [4;2;0;1];;
let deux20 = mult_2l deux10 deux10;;
let deux40 = mult_2l deux20 deux20;;
let deux80 = mult_2l deux40 deux40;;
let deux160 = mult_2l deux80 deux80;;
let deux320 = mult_2l deux160 deux160;;
let deux640 =  mult_2l deux320 deux320;;
let deux960 = mult_2l deux640 deux320;;
let deux1000 = mult_2l deux960 deux40;;

let sum l = List.fold_left (fun a b -> a + b) 0 l in
    sum deux1000;;
  
