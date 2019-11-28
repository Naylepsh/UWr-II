fn solution(n: f64) -> f64 {
    (2.0*n).round() / 2.0
}

#[test]
fn test1() {
    assert_eq!(solution(4.2), 4.0);
}

#[test]
fn test2() {
    assert_eq!(solution(4.1), 4.0);
}

#[test]
fn test3() {
    assert_eq!(solution(4.3), 4.5);
}

#[test]
fn test4() {
    assert_eq!(solution(4.4), 4.5);
}

#[test]
fn test5() {
    assert_eq!(solution(4.5), 4.5);
}

#[test]
fn test6() {
    assert_eq!(solution(4.6), 4.5);
}

#[test]
fn test7() {
    assert_eq!(solution(4.7), 4.5);
}

#[test]
fn test8() {
    assert_eq!(solution(4.8), 5.0);
}

#[test]
fn test9() {
    assert_eq!(solution(4.9), 5.0);
}

#[test]
fn test10() {
    assert_eq!(solution(5.2), 5.0);
}
