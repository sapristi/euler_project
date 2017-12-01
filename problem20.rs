use std::env;

fn decomp (n: i32) -> Vec<i32> {
  if n < 10 {
    if n==0 { return Vec::new();}
    else {return vec![n];}
  }
  else
  {
    let mut v = decomp(n/10);
    v.push(n%10);
    return v;  }
  
}

fn mult (n:i32, mut bn: Vec<i32>, rem: i32) -> Vec<i32>{
  if bn.is_empty() {

    return decomp(rem);

  } else {

    let k = bn.pop().unwrap();
    let m = k*n + rem;
    let mut res = mult(n, bn, m/10);
    res.push(m % 10);
    return res;
  }
}


fn mult_tail (n:i32, mut bn: Vec<i32>, rem: i32, mut res:Vec<i32>) -> Vec<i32> {
  if bn.is_empty() {
    let mut rem_a = decomp(rem);
    rem_a.reverse();
    res.append(rem_a.as_mut());
    res.reverse();
    return res
  } else {

    let k = bn.pop().unwrap();
    let m = k*n + rem;
    res.push(m%10);
    
    return mult_tail(n, bn, m/10, res);
    
  }
}
fn fact(n : i32) -> Vec<i32> {
  if n == 1 {
    return vec![1];
  } else {

    return mult(n, fact(n-1), 0);
  }
}

fn fact_tail(n:i32, res:Vec<i32>) -> Vec<i32> {
  if n == 1 {return res;}
  else {
    return fact_tail(n-1, mult_tail(n, res, 0, Vec::new()));}
}

fn main () {
  let args: Vec<String> = env::args().collect();
  let n :i32 = (args[1]).parse().unwrap();
  
  let a = mult(n, decomp(5), 0);
  println!("{:?}", a);
  
  let aa = mult_tail(n, decomp(5), 0, Vec::new());
  println!("{:?}", aa);
  
  let b = mult(5, a, 0);
  println!("{:?}", b);
  
  let bb = mult_tail(5, aa, 0, Vec::new());
  println!("{:?}", bb);

  
  println!("{:?}", fact(n));
  println!("{:?}", fact_tail(n, vec![1]));
}
