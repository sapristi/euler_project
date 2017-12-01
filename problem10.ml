let rec test_prime n l =
  match l with
  | h :: t -> if n mod h = 0 then false else test_prime n t
  | [] -> true
    in
    let res = ref 2 and primes = ref [2] in
    for i = 1 to 1000000 do
      let j = 2*i+1 in
      if test_prime j !primes
      then
        begin
          res := !res + j;
          primes := (!primes)@[j];
          print_int j;
          print_newline ();
        end;
    done;
    !res
;;
