fn main() {
    even_numbers(&vec!(1, 2, 3, 4, 5, 6, 7, 8, 9), 3);
}

fn even_numbers(array: &Vec<i32>, number: usize) -> Vec<i32> {
  let mut digits = array.iter().cloned().filter(|digit| digit % 2 == 0).rev().take(number).collect::<Vec<i32>>();
  digits.reverse();
  digits
}

#[test]
fn test1() {
    assert_eq!(even_numbers(&vec!(1, 2, 3, 4, 5, 6, 7, 8, 9), 3), vec!(4, 6, 8));
}

#[test]
fn test2() {
    assert_eq!(even_numbers(&vec!(-22, 5, 3, 11, 26, -6, -7, -8, -9, -8, 26), 2), vec!(-8, 26));
}

#[test]
fn test3() {
    assert_eq!(even_numbers(&vec!(6, -25, 3, 7, 5, 5, 7, -3, 23), 1), vec!(6));
}

#[test]
fn test4() {
    assert_eq!(even_numbers(&vec!(6, -24, 3, 7, 5, 5, 7, -3, 23), 2), vec!(6, -24));
}

#[test]
fn test5() {
    assert_eq!(even_numbers(&vec!(6, -25, 8, 7, 5, 5, 7, -3, 23), 2), vec!(6, 8));
}

#[test]
fn test6() {
    assert_eq!(even_numbers(&vec!(6, -25, 3, 7, 5, 5, 8, -3, 24), 3), vec!(6, 8, 24));
}

#[test]
fn test7() {
    assert_eq!(even_numbers(&vec!(0, 1, 2, 4, 8), 4), vec!(0,2,4,8));
}

#[test]
fn test8() {
    assert_eq!(even_numbers(&vec!(0, -1, -2, -4, -8), 4), vec!(0,-2,-4,-8));
}

#[test]
fn test9() {
    assert_eq!(even_numbers(&vec!(1, 11, 111, 1111, 1111), 1), vec!());
}

#[test]
fn test10() {
    assert_eq!(even_numbers(&vec!(1, 11, 111, 1111, 1111), 1+2), vec!());
}