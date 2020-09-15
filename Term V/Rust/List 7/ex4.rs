fn main() {
    println!("Hello, world!");
}

fn john(n: i32) -> Vec<i32> {
    foo(n).1
}
fn ann(n: i32) -> Vec<i32> {
    foo(n).0
}
fn sum_john(n: i32) -> i32 {
    john(n).iter().sum()
}
fn sum_ann(n: i32) -> i32 {
    ann(n).iter().sum()
}
fn foo(n: i32) -> (Vec<i32>, Vec<i32>) {
    let mut a_vec = vec![1];
    let mut j_vec = vec![0];
    
    for i in 1..n {
        let at_katas = *a_vec.last().unwrap();
        let jt_katas = *j_vec.last().unwrap();
        j_vec.push(i - a_vec[jt_katas as usize]);
        a_vec.push(i - j_vec[at_katas as usize]);
    }
    
    (a_vec, j_vec)
}

fn test_john(n: i32, exp: Vec<i32>) -> () {
    assert_eq!(john(n), exp)
}
fn test_ann(n: i32, exp: Vec<i32>) -> () {
    assert_eq!(ann(n), exp)
}
fn test_sum_john(n: i32, exp: i32) -> () {
    assert_eq!(sum_john(n), exp)
}
fn test_sum_ann(n: i32, exp: i32) -> () {
    assert_eq!(sum_ann(n), exp)
}

#[test]
fn test_test_john0() {
    test_john(0, vec![0]);
}

#[test]
fn test_test_john1() {
    test_john(11, vec![0, 0, 1, 2, 2, 3, 4, 4, 5, 6, 6]);
}

#[test]
fn test_test_john2() {
    test_john(14, vec![0, 0, 1, 2, 2, 3, 4, 4, 5, 6, 6, 7, 7, 8]);
}

#[test]
fn test_test_ann0() {
    test_ann(0, vec![1]);
}

#[test]
fn test_test_ann1() {
    test_ann(6, vec![1, 1, 2, 2, 3, 3]);
}

#[test]
fn test_test_ann2() {
    test_ann(15, vec![1, 1, 2, 2, 3, 3, 4, 5, 5, 6, 6, 7, 8, 8, 9]);
}

#[test]
fn test_test_sum_john1() {
    test_sum_john(75, 1720);
}

#[test]
fn test_test_sum_john2() {
    test_sum_john(78, 1861);
}

#[test]
fn test_test_sum_ann1() {
    test_sum_ann(150, 6930);
}

#[test]
fn test_test_sum_ann2() {
    test_sum_ann(115, 4070);
}

