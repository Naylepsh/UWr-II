fn count_bits(n: i64) -> u32 {
    format!("{:b}", n).matches("1").count() as u32
}

#[test]
fn test1() {
    assert_eq!(count_bits(204), 4);
}

#[test]
fn test2() {
    assert_eq!(count_bits(154), 4);
}

#[test]
fn test3() {
    assert_eq!(count_bits(110), 5);
}


#[test]
fn test4() {
    assert_eq!(count_bits(73), 3);
}

#[test]
fn test5() {
    assert_eq!(count_bits(24), 2);
}

#[test]
fn test6() {
    assert_eq!(count_bits(137), 3);
}


#[test]
fn test7() {
    assert_eq!(count_bits(70), 3);
}

#[test]
fn test8() {
    assert_eq!(count_bits(255), 8);
}

#[test]
fn test9() {
    assert_eq!(count_bits(123), 6);
}

#[test]
fn test10() {
    assert_eq!(count_bits(55), 5);
}



