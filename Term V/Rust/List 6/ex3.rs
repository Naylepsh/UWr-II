fn print(n: i32) -> Option<String> {
    if (n < 0) || (n % 2 == 0) {
        return None;
    }
    let mut diamond = String::new();
    for i in 0..n/2 {
        diamond = format!("{}{}{}\n",
                          diamond,
                          " ".repeat((n/2 - i) as usize),
                          "*".repeat((2*i+1) as usize));
    }
    diamond = format!("{}{}\n",
                      diamond,
                      "*".repeat(n as usize));
    for i in (0..n/2).rev() {
        diamond = format!("{}{}{}\n",
                          diamond,
                          " ".repeat((n/2 - i) as usize),
                          "*".repeat((2*i+1) as usize));
    }
    return Some(diamond);
}

#[test]
fn test1() {
    assert_eq!(print(1), Some("*\n".to_string()));
}

#[test]
fn test2() {
    assert_eq!(print(2), None);
}

#[test]
fn test3() {
    assert_eq!(print(3), Some(" *\n***\n *\n".to_string()));
}

#[test]
fn test4() {
    assert_eq!(print(5), Some("  *\n ***\n*****\n ***\n  *\n".to_string()));
}

#[test]
fn test5() {
    assert_eq!(print(7), Some("   *\n  ***\n *****\n*******\n *****\n  ***\n   *\n".to_string()));
}

#[test]
fn test6() {
    assert_eq!(print(9), Some("    *\n   ***\n  *****\n *******\n*********\n *******\n  *****\n   ***\n    *\n".to_string()));
}

#[test]
fn test7() {
    assert_eq!(print(11), Some("     *\n    ***\n   *****\n  *******\n *********\n***********\n *********\n  *******\n   *****\n    ***\n     *\n".to_string()));
}

#[test]
fn test8() {
    assert_eq!(print(13), Some("      *\n     ***\n    *****\n   *******\n  *********\n ***********\n*************\n ***********\n  *********\n   *******\n    *****\n     ***\n      *\n".to_string()));
}

#[test]
fn test9() {
    assert_eq!(print(100000), None);
}

#[test]
fn test10() {
    assert_eq!(print(-1), None);
}