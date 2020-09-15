fn string_to_number(s: &str) -> i32 {
  s.parse::<i32>().unwrap()
}

#[test]
fn test1() {
    assert_eq!(string_to_number("1234"), 1234);
    
}

#[test]
fn test2() {
    assert_eq!(string_to_number("1405"), 1405);
}

#[test]
fn test3() {
    assert_eq!(string_to_number("605"), 605);
}

#[test]
fn test4() {
    assert_eq!(string_to_number("-7"), -7);
}

#[test]
fn test5() {
    assert_eq!(string_to_number("0"), 0);
}


    
    