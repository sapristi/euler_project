let s = "75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23";;


let split_on_char c s =
  let rec aux s res = 
    try
      let i = String.index s c in
      aux (String.sub s (i+1) ((String.length s)-i-1)) (String.sub s 0 i :: res)
    with
    | Not_found -> s::res
  in
  aux s [];;

  let lines = Array.of_list (split_on_char '\n' s);;
  let striangle = Array.map (fun s -> Array.of_list (split_on_char ' ' s)) lines;;
  let triangle = Array.map (Array.map int_of_string) striangle;;

let l = ref 0 and r = ref 0 in
    for i= 1 to 14 do
      for j= 0 to 14-i do
        l := triangle.(i-1).(j);
        r := triangle.(i-1).(j+1);
        triangle.(i).(j) <- triangle.(i).(j) + max !l !r; 
      done
    done;

    triangle;;
