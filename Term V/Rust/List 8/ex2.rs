fn main() {
    println!("Hello, world!");
}

fn alphabet_position(text: &str) -> String {
    text
    .to_lowercase()
    .chars()
    .filter(|c| *c >= 'a' && *c <= 'z')
    .map(|c| (c as u8 - ('a' as u8) + 1).to_string())
    .collect::<Vec<String>>().join(" ")
}


#[test]
fn test1() {
    assert_eq!(
        alphabet_position("The sunset sets at twelve o' clock."),
        "20 8 5 19 21 14 19 5 20 19 5 20 19 1 20 20 23 5 12 22 5 15 3 12 15 3 11".to_string()
    );
}

#[test]
fn test2() {
    assert_eq!(
        alphabet_position("The narwhal bacons at midnight."),
        "20 8 5 14 1 18 23 8 1 12 2 1 3 15 14 19 1 20 13 9 4 14 9 7 8 20".to_string()
    );
}

#[test]
fn test3() {
    assert_eq!(
        alphabet_position("abcdefghi"),
        "1 2 3 4 5 6 7 8 9".to_string()
    );
}

#[test]
fn test4() {
    assert_eq!(
        alphabet_position("a b c d e f g h i"),
        "1 2 3 4 5 6 7 8 9".to_string()
    );
}

#[test]
fn test5() {
    assert_eq!(
        alphabet_position("abecadlo"),
        "1 2 5 3 1 4 12 15".to_string()
    );
}

#[test]
fn test6() {
    assert_eq!(
        alphabet_position("a b e c a d l o"),
        "1 2 5 3 1 4 12 15".to_string()
    );
}

#[test]
fn test7() {
    assert_eq!(
        alphabet_position("4b3c4dl0"),
        "2 3 4 12".to_string()
    );
}

#[test]
fn test8() {
    assert_eq!(
        alphabet_position("hahaha.. HAHA!"),
        "8 1 8 1 8 1 8 1 8 1".to_string()
    );
}

#[test]
fn test9() {
    assert_eq!(
        alphabet_position("BIG"),
        "2 9 7".to_string()
    );
}

#[test]
fn test10() {
    assert_eq!(
        alphabet_position("dDdDdd"),
        "4 4 4 4 4 4".to_string()
    );
}