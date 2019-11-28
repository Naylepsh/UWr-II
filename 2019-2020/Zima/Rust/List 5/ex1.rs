fn xo(string: &'static str) -> bool {
  let mut acc = 0;
  for ch in string.to_lowercase().chars() {
    match ch {
      'x' => acc += 1,
      'o' => acc -= 1,
      _ => ()
    }
  }
  acc == 0
}

// XO("zpzpzpp") => true // when no 'x' and 'o' is present should return true
// XO("zzoo") => false

#[test]
fn test1() {
    assert_eq!(xo("ooxx"), true);
}

#[test]
fn test2() {
    assert_eq!(xo("xooxx"), false);
}

#[test]
fn test3() {
    assert_eq!(xo("ooxXm"), true);
}

#[test]
fn test4() {
    assert_eq!(xo("zpzpzpp"), true);
}

#[test]
fn test5() {
    assert_eq!(xo("zoo"), false);
}

#[test]
fn test6() {
    assert_eq!(xo("oxooxo"), false);
}

#[test]
fn test7() {
    assert_eq!(xo("ooxxooxx"), true);
}

#[test]
fn test8() {
    assert_eq!(xo("ooxxxxooo"), false);
}

#[test]
fn test9() {
    assert_eq!(xo("ooxxXDDDDDDDDD"), false);
}

#[test]
fn test10() {
    assert_eq!(xo("Boze jak smiesznie"), false);
}