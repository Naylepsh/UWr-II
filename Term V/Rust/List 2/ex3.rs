fn summy(strng: &str) -> i32 {
    strng.split(' ').fold(0, |a,b| a+b.parse::<i32>().unwrap())
}

#[test]
fn test1() {
    assert_eq!(summy("1 2 3"), 6);
}

#[test]
fn test2() {
    assert_eq!(summy("1 2 3 4"), 10);
}

#[test]
fn test3() {
    assert_eq!(summy("1 2 3 4 5"), 15);
}

#[test]
fn test4() {
    assert_eq!(summy("10 10"), 20);
}

#[test]
fn test5() {
    assert_eq!(summy("0 0"), 0);
}

#[test]
fn test6() {
    assert_eq!(summy("26 27 7 13 8 30 24 9"), 144);
}

#[test]
fn test7() {
    assert_eq!(summy("10 2 6 6 24 6 13 26 23 7 1 1 12"), 137);
}

#[test]
fn test8() {
    assert_eq!(summy("16 23 12 20 6 4 10 30 20 18 30 12"), 201);
}

#[test]
fn test9() {
    assert_eq!(summy("17 29 6 25 14 4 12 9 14"), 130);
}

#[test]
fn test10() {
    assert_eq!(summy("1 27 10 24 24 27"), 113);
}



