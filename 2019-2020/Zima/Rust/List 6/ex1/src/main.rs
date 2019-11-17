fn main() {
    println!("{:?}", encode("abcdefg".to_string(), 0));
}

fn encode(msg: String, n: i32) -> Vec<i32> {
    msg.chars()
        .zip(n.to_string().chars().cycle())
        .map(|(x, y)|  x as i32 - ('a' as i32) + 1 + 
                      (y as i32) - ('0' as i32))
        .collect::<Vec<i32>>()
}


#[test]
fn test1() {
    assert_eq!(encode("scout".to_string(), 1939), vec![20, 12, 18, 30, 21]);
}

#[test]
fn test2() {
    assert_eq!(encode("masterpiece".to_string(), 1939), vec![14, 10, 22, 29, 6, 27, 19, 18, 6, 12, 8]);
}


#[test]
fn test3() {
    assert_eq!(encode("vegane".to_string(), 32), vec![25, 7, 10, 3, 17, 7]);
}

#[test]
fn test4() {
    assert_eq!(encode("monkas".to_string(), 42), vec![17, 17, 18, 13, 5, 21]);
}

#[test]
fn test5() {
    assert_eq!(encode("johndoe".to_string(), 1945), vec![11, 24, 12, 19, 5, 24, 9]);
}

#[test]
fn test6() {
    assert_eq!(encode("janedoe".to_string(), 1945), vec![11, 10, 18, 10, 5, 24, 9]);
}

#[test]
fn test7() {
    assert_eq!(encode("twice".to_string(), 2015), vec![22, 23, 10, 8, 7]);
}

#[test]
fn test8() {
    assert_eq!(encode("".to_string(), 1945), vec![]);
}

#[test]
fn test9() {
    assert_eq!(encode("doom".to_string(), 1993), vec![5, 24, 24, 16]);
}

#[test]
fn test10() {
    assert_eq!(encode("abcdefg".to_string(), 0), vec![1, 2, 3, 4, 5, 6, 7]);
}