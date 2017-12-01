let roads = Array.make_matrix 21 21 0;;

  for i = 0 to 20 do 
    roads.(i).(0) <- 1;
    roads.(0).(i) <- 1;
  done;;

  for i = 1 to 20 do
    for j = 1 to 20 do
      roads.(i).(j) <- roads.(i-1).(j) + roads.(i).(j-1)
    done;
  done;;
    
roads.(20).(20);;
roads;;
