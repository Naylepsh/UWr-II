fn comp(a: Vec<i64>, b: Vec<i64>) -> bool {
    let mut c = a.iter().map(|x| x.pow(2)).collect::<Vec<i64>>();
    let mut d = b;
    c.sort();
    d.sort();
    c == d
}

#[test]
fn test1() {
    assert_eq!(comp(vec![45, 95, 80, 89, 91, 23], vec![2025, 9025, 6400, 7921, 8281, 529]), true);
}

#[test]
fn test2() {
    assert_eq!(comp(vec![32, 21, 29, 70, 40, 41], vec![1024, 441, 841, 4900, 1600, 1681]), true);
}

#[test]
fn test3() {
    assert_eq!(comp(vec![13, 24, 95, 11, 46, 73, 1, 52], vec![169, 576, 9025, 121, 2116, 5329, 1, 2704]), true);
}

#[test]
fn test4() {
    assert_eq!(comp(vec![68, 60, 75, 29, 41, 89, 26, 84, 92], vec![4624, 3600, 5625, 841, 1681, 7921, 676, 7056, 8464]), true);
}

#[test]
fn test5() {
    assert_eq!(comp(vec![90, 58, 96, 32, 19, 1, 0], vec![8100, 3364, 9216, 1024, 361, 1, 0]), true);
}

#[test]
fn test6() {
    assert_eq!(comp(vec![90, 58, 96, 32, 19, 1, 0], vec![8101, 3364, 9216, 1024, 361, 1, 0]), false);
}

#[test]
fn test7() {
    assert_eq!(comp(vec![90, 58, 96, 32, 19, 1, 0], vec![8100, 3365, 9216, 1024, 361, 1, 0]), false);
}


#[test]
fn test8() {
    assert_eq!(comp(vec![68, 60, 75, 29, 41, 89, 26, 84, 92], vec![46241, 3600, 5625, 841, 1681, 7921, 676, 7056, 8464]), false);
}

#[test]
fn test9() {
    assert_eq!(comp(vec![1], vec![1]), true);
}

#[test]
fn test10() {
    assert_eq!(comp(vec![0], vec![0]), true);
}
