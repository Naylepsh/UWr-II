fn main() {
    println!("{:?}", last_digit("3715290469715693021198967285016729344580685479654510946723", "68"));
}

fn last_digit(str1: &str, str2: &str) -> i32 {
  if str2 == "0"{
    return 1;
  }
  let mut _exp: u32 = 0; 
  if str2.len() == 1{
      _exp = str2.chars().next().unwrap().to_digit(10).unwrap();
  } else{
    let last_two = str2.chars().rev().take(2).collect::<String>().chars().rev().collect::<String>().parse::<u32>().unwrap();
      _exp = if last_two % 4 == 0 {4} else {last_two % 4};
  } 
  let last_digit = str1.chars().last().unwrap().to_digit(10).unwrap() as i32;
  last_digit.pow(_exp) % 10
}
