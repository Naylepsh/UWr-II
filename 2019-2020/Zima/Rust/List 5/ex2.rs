use std::iter::FromIterator;

fn descending_order(x: u64) -> u64 {
    let mut digits = x.to_string().chars().collect::<Vec<char>>();
    digits.sort();
    digits.reverse();
    String::from_iter(digits).parse::<u64>().unwrap()
}

#[test]
fn test1() {
    assert_eq!(descending_order(0), 0);
}

#[test]
fn test2() {
    assert_eq!(descending_order(1), 1);
}

#[test]
fn test3() {
    assert_eq!(descending_order(15), 51);
}

#[test]
fn test4() {
    assert_eq!(descending_order(1021), 2110);
}

#[test]
fn test5() {
    assert_eq!(descending_order(123456789), 987654321);
}

#[test]
fn test6() {
    assert_eq!(descending_order(145263), 654321);
}

#[test]
fn test7() {
    assert_eq!(descending_order(1254859723), 9875543221);
}

#[test]
fn test8() {
    assert_eq!(descending_order(125485972399), 999875543221);
}

#[test]
fn test9() {
    assert_eq!(descending_order(123321), 332211);
}

#[test]
fn test10() {
    assert_eq!(descending_order(10001), 11000);
}
