fn last_digit(lst: &[u64]) -> u64 {
    if lst.is_empty() { return 1; }
    lst
    .into_iter()
    .cloned()
    .rev()
    .fold(1, |acc, val| foo(val).pow(bar(acc) as u32)) % 10
}
  
fn foo(val: u64) -> u64 {
    if val < 20 {
        return val;
    } else {
        return val % 20 + 20;
    }
}

fn bar(acc: u64) -> u64{
    if acc < 4 {
        return acc;
    } else {
        return acc % 4 + 4;
    }
}

#[test]
fn test1() {
    assert_eq!(last_digit(&[]), 1);
}

#[test]
fn test2() {
    assert_eq!(last_digit(&[0]), 0);
}

#[test]
fn test3() {
    assert_eq!(last_digit(&[0,0]), 1);
}

#[test]
fn test4() {
    assert_eq!(last_digit(&[0,0,0]), 0);
}

#[test]
fn test5() {
    assert_eq!(last_digit(&[1, 0, 0, 1, 1, 1, 0, 1, 0, 0]), 1);
}

#[test]
fn test6() {
    assert_eq!(last_digit(&[947916, 773927, 882572, 669582, 735565, 508561, 560258, 265538, 175717, 500538]), 6);
}

#[test]
fn test7() {
    assert_eq!(last_digit(&[0, 2, 1, 2, 2, 0, 2, 0, 2, 2]), 0);
}

#[test]
fn test8() {
    assert_eq!(last_digit(&[50788, 303739, 42756, 157232, 653800, 414451, 249503, 698094, 283978, 277288]), 8);
}

#[test]
fn test9() {
    assert_eq!(last_digit(&[2, 1, 2, 0, 1, 1, 2, 1, 0, 1]), 2);
}

#[test]
fn test10() {
    assert_eq!(last_digit(&[1, 1, 0, 0, 1, 0, 0, 0, 1, 0]), 1);
}
