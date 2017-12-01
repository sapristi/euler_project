let file = "p067_triangle.txt";;


let read_file () = 
  let ic = open_in file and input_list = ref [] in
  try
    while true do
      input_list := input_line ic :: !input_list
    done;
    !input_list;
  with e -> 
    close_in ic;
    !input_list
;;


let input = read_file ();;  

  
let split_on_char c s =
  let rec aux s res = 
    try
      let i = String.index s c in
      aux (String.sub s (i+1) ((String.length s)-i-1)) (String.sub s 0 i :: res)
    with
    | Not_found -> s::res
  in
  aux s [];;


  let striangle = Array.map (fun s -> Array.of_list (split_on_char ' ' s)) (Array.of_list input);;
  let triangle = Array.map (Array.map int_of_string) striangle;;


    let l = ref 0 and r = ref 0 in
    for i= 1 to 99 do
      for j= 0 to 99-i do
        l := triangle.(i-1).(j);
        r := triangle.(i-1).(j+1);
        triangle.(i).(j) <- triangle.(i).(j) + max !l !r; 
      done
    done;

    triangle;;

      triangle.(99).(0);;
        
