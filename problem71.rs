use std::env;



fn gcd(n:u64, m:u64) -> u64{
  if n==m {n}
  else {
    if n>m {gcd(n-m,m)}
    else {gcd(m-n,n)}
  }
}





fn closest_to_target(k:u64, d: u64) -> (u64,u64) {
  let mut n = k;
  let mut res = 0;
  loop {
    loop {
      n += 1;
      if gcd(d, n) == 1
      {break;}
    }
    if 7*n >= 3*d
    { return (res,d);}
    else {res = n;}
  }
}


fn closest_to_target_rev(start:u64, d: u64) -> (u64,u64) {
  let mut n = start;
  loop {
    loop {
      n -= 1;
      if gcd(d, n) == 1
      {break;}
      if n==0 {break;}
    }
    if 7*n < 3*d
    { return (n,d);}
    if n==0 {return (0,d);}
  }
}

fn comp (n1:u64,d1:u64,n2:u64,d2:u64) -> bool {
  return (n1 as f64 / d1 as f64)>(n2 as f64 / d2 as f64)
}
fn main () {
  let args: Vec<String> = env::args().collect();
  let k :u64 = (args[1]).parse().unwrap();

  let mut d : u64 = 1;
  let mut n : u64 = 0;
  
  for i in (k-20)..k {

    let imm_upper = (i as f64 * 3./7.) as u64 +1;
    
    let (new_n,new_d) = closest_to_target_rev(imm_upper,i);
    if comp(new_n,new_d,n,d) { 
      n = new_n;
      d = new_d;
    }
  }

  println!("{}/{}",n, d);
}
