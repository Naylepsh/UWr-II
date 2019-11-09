fn find_digit(num: i32, nth: i32) -> i32 {
    if nth <= 0 { 
        return -1;
    } else { 
        let mut x = num.abs();
        for _ in 1..nth {
            x /= 10;
        }
        return x % 10;
    }
}

#[test]
fn test1() {
    assert_eq!(find_digit(5673, 4), 5);
}

#[test]
fn test2() {
    assert_eq!(find_digit(129, 2), 2);
}

#[test]
fn test3() {
    assert_eq!(find_digit(-2825, 3), 8);
}

#[test]
fn test4() {
    assert_eq!(find_digit(-456, 4), 0);
}

#[test]
fn test5() {
    assert_eq!(find_digit(0, 20), 0);
}

#[test]
fn test6() {
    assert_eq!(find_digit(65, 0), -1);
}

#[test]
fn test7() {
    assert_eq!(find_digit(24, -8), -1);
}

#[test]
fn test8() {
    assert_eq!(find_digit(420, -666), -1);
}

#[test]
fn test9() {
    assert_eq!(find_digit(100000000, 7), 0);
}

#[test]
fn test10() {
    assert_eq!(find_digit(240, 1), 0);
}