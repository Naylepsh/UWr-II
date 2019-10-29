fn get_count(string: &str) -> usize {
  string.chars().filter(|&letter| "aeoui".contains(letter)).count()
}

#[test]
fn test1() {
  assert_eq!(get_count("abracadabra"), 5);
}

#[test]
fn test2() {
  assert_eq!(get_count("aaaaaa"), 6);
}

#[test]
fn test3() {
  assert_eq!(get_count(""), 0);
}

#[test]
fn test4() {
  assert_eq!(get_count("skrrrrrrrrrr"), 0);
}

#[test]
fn test5() {
  assert_eq!(get_count("reeee"), 4);
}

#[test]
fn test6() {
  assert_eq!(get_count("monkas"), 2);
}

#[test]
fn test7() {
  assert_eq!(get_count("pepega"), 3);
}

#[test]
fn test8() {
  assert_eq!(get_count("qqqq"), 0);
}

#[test]
fn test9() {
  assert_eq!(get_count("mmmmmmaaaammmmmjhkjhkjhkji"), 5);
}

#[test]
fn test10() {
  assert_eq!(get_count("zzzzzzzzzzzzzzzzzzzzzzzzzzhghghfhgfhgdsg"), 0);
}