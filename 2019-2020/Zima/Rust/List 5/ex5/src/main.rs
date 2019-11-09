use std::cmp;

fn dbl_linear(n: u32) -> u32{
    let mut ys = Vec::new();
    let mut zs = Vec::new();
    let mut x = 1;
    let mut prev = 0;
    for _ in 0..(n+1) {
        let y = 2*x+1;
        let z = 3*x+1;
        ys.push(y);
        zs.push(z);
        
        while x == prev {
            x = cmp::min(ys[0], zs[0]);
            if x == ys[0] {
                ys.remove(0);
            } else {
                zs.remove(0);
            }
        }
        prev = x;
    }
    x
}

#[test]
fn test1() {
    assert_eq!(dbl_linear(0), 1);
}

#[test]
fn test2() {
    assert_eq!(dbl_linear(1), 3);
}

#[test]
fn test3() {
    assert_eq!(dbl_linear(2), 4);
}

#[test]
fn test4() {
    assert_eq!(dbl_linear(3), 7);
}

#[test]
fn test5() {
    assert_eq!(dbl_linear(4), 9);
}

#[test]
fn test6() {
    assert_eq!(dbl_linear(5), 10);
}

#[test]
fn test7() {
    assert_eq!(dbl_linear(6), 13);
}

#[test]
fn test8() {
    assert_eq!(dbl_linear(7), 15);
}

#[test]
fn test9() {
    assert_eq!(dbl_linear(8), 19);
}

#[test]
fn test10() {
    assert_eq!(dbl_linear(14), 39);
}