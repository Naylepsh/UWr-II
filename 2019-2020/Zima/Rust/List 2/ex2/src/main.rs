fn longest(a1: &str, a2: &str) -> String {
    let mut temp = format!("{}{}", a1, a2).chars().collect::<Vec<char>>();
    temp.sort();
    temp.dedup();
    temp.iter().collect()
}

#[test]
fn test1() {
    assert_eq!(longest("aaaaaaa", "aaaaaaa"), "a");
}

#[test]
fn test2() {
    assert_eq!(longest("bbbbddddccaaaaaaa", "aaaaaaa"), "abcd");
}

#[test]
fn test3() {
    assert_eq!(longest("cba", "fed"), "abcdef");
}

#[test]
fn test4() {
    assert_eq!(longest("opopopopuuu", "aaaaaaa"), "aopu");
}

#[test]
fn test5() {
    assert_eq!(longest("rxfzrac", "myljosmwiltywqsx"), "acfijlmoqrstwxyz");
}

#[test]
fn test6() {
    assert_eq!(longest("hhyhbv", "nmgweeqojzhwlxqlkvyu"), "beghjklmnoquvwxyz");
}

#[test]
fn test7() {
    assert_eq!(longest("goxpiblnxfbdqijkdjgs", "znmt"), "bdfgijklmnopqstxz");
}

#[test]
fn test8() {
    assert_eq!(longest("bekwosomtqnujkvffsii", "wluqstm"), "befijklmnoqstuvw");
}

#[test]
fn test9() {
    assert_eq!(longest("wmlmdarmc", "olr"), "acdlmorw");
}

#[test]
fn test10() {
    assert_eq!(longest("bjikowgpzosvafidgmiu", "lsdszssb"), "abdfgijklmopsuvwz");
}



