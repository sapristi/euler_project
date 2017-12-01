let rec decomp n =
  if n < 10
  then
    if n = 0
    then  []
    else [n]
  else 
    (n mod 10) :: (decomp (n/10))
                 

let rec mul (n : int) (bn : int list) (rem : int) : int list =
  match bn with
  | k :: bn' ->
     let m = k * n + rem in
     (m mod 10) :: (mul n bn' (m/10))
  | [] -> decomp rem


let rec fact n =
  if n = 1
  then [1]
  else
    mul n (fact (n-1)) 0
;;

List.fold_left (fun a b -> a+b) 0 (fact 100);;
    
