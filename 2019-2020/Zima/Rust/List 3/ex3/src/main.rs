struct Cipher {
  transform: Vec<(char, char)>
}

impl Cipher {
  fn new(map1: &str, map2: &str) -> Cipher {
    Cipher {
      transform: map1.chars().zip(map2.chars()).collect()     
    }
  }
  
  fn encode(&self, string: &str) -> String {
    string.chars().map(|c| self.transform.iter().find(|x| x.0 == c).unwrap_or(&(c,c)).1).collect()
  }
  
  fn decode(&self, string: &str) -> String {
    string.chars().map(|c| self.transform.iter().find(|x| x.1 == c).unwrap_or(&(c,c)).0).collect()
  }
}

#[test]
fn examples() {
  let map1 = "abcdefghijklmnopqrstuvwxyz";
  let map2 = "etaoinshrdlucmfwypvbgkjqxz";

  let cipher = Cipher::new(map1, map2);
  
  assert_eq!(cipher.encode("abc"), "eta");
}

#[test]
fn test2() {
  let map1 = "abcdefghijklmnopqrstuvwxyz";
  let map2 = "etaoinshrdlucmfwypvbgkjqxz";

  let cipher = Cipher::new(map1, map2);
  
  assert_eq!(cipher.encode("xyz"), "qxz");
}

#[test]
fn test3() {
    let map1 = "abcdefghijklmnopqrstuvwxyz";
  let map2 = "etaoinshrdlucmfwypvbgkjqxz";

  let cipher = Cipher::new(map1, map2);
  
  assert_eq!(cipher.decode("eirfg"), "aeiou");
}

#[test]
fn test4() {
    let map1 = "abcdefghijklmnopqrstuvwxyz";
  let map2 = "etaoinshrdlucmfwypvbgkjqxz";

  let cipher = Cipher::new(map1, map2);
  
  assert_eq!(cipher.decode("erlang"), "aikcfu");
}

#[test]
fn test5() {
  let map1 = "";
  let map2 = "etaoinshrdlucmfwypvbgkjqxz";

  let cipher = Cipher::new(map1, map2);
  
  assert_eq!(cipher.decode("qwe"), "qwe");
}


#[test]
fn test6() {
  let map1 = "";
  let map2 = "";

  let cipher = Cipher::new(map1, map2);
  
  assert_eq!(cipher.decode("qwe"), "qwe");
}

#[test]
fn test7() {
  let map1 = "";
  let map2 = "etaoinshrdlucmfwypvbgkjqxz";

  let cipher = Cipher::new(map1, map2);
  
  assert_eq!(cipher.decode("qwe"), "qwe");
}

#[test]
fn test8() {
  let map1 = "aa";
  let map2 = "bc";

  let cipher = Cipher::new(map1, map2);
  
  assert_eq!(cipher.encode("aaa"), "bbb");
}

#[test]
fn test9() {
    let map1 = "()";
  let map2 = "[]";

  let cipher = Cipher::new(map1, map2);
  
  assert_eq!(cipher.encode("hello_world(1)"), "hello_world[1]");
}

#[test]
fn test10() {
  let map1 = "1234567890";
  let map2 = "0123456789";

  let cipher = Cipher::new(map1, map2);
  
  assert_eq!(cipher.encode("h4x0r"), "h3x9r");
}