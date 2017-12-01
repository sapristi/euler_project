use std::env;


fn main () {

  let args: Vec<String> = env::args().collect();
  let n :usize = (args[1]).parse().unwrap();

  let res = Vec::with_capacity(n);;

}
