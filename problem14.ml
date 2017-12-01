let seq_len = Array.make 100000000 0;;

let rec seq_test n =
  print_string (" "^(string_of_int n));
  if n != 1
  then
    if n mod 2 = 0
    then seq_test (n/2)
    else seq_test (3*n+1)
;;
  
let rec seq n l=
  if n = 1
  then l+1
  else if n < Array.length seq_len
  then
    if seq_len.(n) != 0
    then l+seq_len.(n)
    else 
      if n mod 2 = 0
      then
        seq (n/2) (l+1)
      else
        seq (3*n+1) (l+1)
  else
    if n mod 2 = 0
    then
      seq (n/2) (l+1)
    else
      seq (3*n+1) (l+1)
;;

         
  for i = 1 to 1000000 do
    seq_len.(i) <- seq i 0
  done
;;
      
      seq_len
    ;;

    let m = ref 0 and n = ref 0 in
        for i = 0 to 1000000 do
          if seq_len.(i) > !m
          then
            begin
              m := seq_len.(i);
              n := i;
            end
        done;
        
!n;;
