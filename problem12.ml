

let rec insert l v = match l with
  | [] -> [v]
  | h :: t ->
     if v > h
     then v::l
     else if h = v
     then l
     else h :: (insert t v)
and merge l1 l2 =
  match l1, l2 with
  | [], [] -> []
  | [], l2 -> l2
  | l1, [] -> l1
  |  h1:: t1, h2::t2 -> 
      if h1 > h2
      then h1 :: merge t1 l2
      else if h1 = h2
      then h1 :: merge t1 t2
      else h2 :: merge l1 t2
;;
  
let find_first_divisor n =
  let lim =  int_of_float (sqrt (float_of_int n)) in
  let d = ref 2 in
  while not ((n mod !d) = 0) do
    incr d;
  done;
  if !d = lim + 1
  then n
  else !d
;;

let rec find_other_divisors n dl =
  match dl with
  | [] -> []
  | h :: t ->
     if n mod h = 0
     then (n/h) :: find_other_divisors n t
     else find_other_divisors n t
;;


let rec find_divisors n divisors=
  if divisors.(n) != []
  then divisors.(n)
  else
    let d = find_first_divisor n in
    if d = n
    then divisors.(n) <- [n;1]
    else
      begin
        let rec_divs = find_divisors (n/d) divisors in
        let other_divs =  List.rev (find_other_divisors n rec_divs) in
        let divs = merge (merge rec_divs other_divs) [n;d] in
        
        divisors.(n) <- divs;
      end;
    divisors.(n);
;;

let divisors = Array.make 1000 [] in
    find_divisors 400 divisors;;

let answer k =
  let divisors = Array.make 100000000 [] in
  let n = ref 2 and t = ref 1 and res = ref 0  and divs = ref [] in
  while !res < k do
    t := !t + !n;
    divs := find_divisors !t divisors;
    res := List.length !divs;
    incr n;
  done;
  (!t, !divs, !res)
;;
                           
answer 500;;
