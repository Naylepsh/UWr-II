fn main() {
    println!("{:?}", dig_pow(1, 0));
}

fn dig_pow(n: i64, p: i32) -> i64 {
    let mut acc = 0_i64;
    let mut power = p as u32;
    for digit in n.to_string().chars() {
        acc += (digit.to_digit(10).unwrap() as i64).pow(power);
        power += 1;
    }
    if acc % n == 0 {
        return acc / n;
    } else {
        return -1;
    }
}

#[test]
fn test1() {
    assert_eq!(dig_pow(123, 4), -1);
}

#[test]
fn test2() {
    assert_eq!(dig_pow(1, 0), 1);
}

#[test]
fn test3() {
    assert_eq!(dig_pow(89, 1), 1);
}

#[test]
fn test4() {
    assert_eq!(dig_pow(695, 2), 2);
}

#[test]
fn test5() {
    assert_eq!(dig_pow(46288, 3), 51);
}

#[test]
fn test6() {
    assert_eq!(dig_pow(46288, 4), -1);
}

#[test]
fn test7() {
    assert_eq!(dig_pow(695, 4), -1);
}

#[test]
fn test8() {
    assert_eq!(dig_pow(89, 2), -1);
}

#[test]
fn test9() {
    assert_eq!(dig_pow(123, 4), -1);
}

#[test]
fn test10() {
    assert_eq!(dig_pow(6, 4), 6_i64.pow(3));
}


