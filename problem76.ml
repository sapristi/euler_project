let rec get_sums_lt (sums : int list list) (bound :int) : (int list list) =
  match sums with
  | [] -> []
  | s:: sums' ->
     match s with
     |h::t->
       if h <= bound
       then s :: get_sums_lt sums' bound
       else []
     |[] -> failwith "empty sum"
     

let count_sum n  =
  let all_sums = Array.make (n+1) [] in
  all_sums.(1) <- [[1]];
  for i = 2 to n do
    let sums = ref [[i]] in
    for j = 1 to i-1 do
      sums := List.fold_left
                (fun res l ->
                  if List.hd l <= (i-j)
                  then 
                    ((i-j)::l)::res
                  else res) !sums
                all_sums.(j)
    
    done;
    all_sums.(i) <- !sums;
  done;
  all_sums
  
let sum_counts (c : int array) (b : int) =
  let res = ref 0 in
  for i = 0 to min ((Array.length c)-1) (b) do
    res := !res + c.(i);
  done;
  !res
  

  
let efficient_count_sum n =
  let all_counts = Array.init (n+1) (fun i -> Array.make (i+1) 0) in
  all_counts.(0).(0)<- 1;
  for i = 1 to n do
    for j = 0 to i-1 do
      all_counts.(i).(i-j) <- sum_counts all_counts.(j) (i-j);
    done;
  done;
  all_counts;;
  
(Array.fold_left (fun a b ->a+b) 0 (efficient_count_sum 100).(100))-1;;
