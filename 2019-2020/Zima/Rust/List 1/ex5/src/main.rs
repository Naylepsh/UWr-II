fn printer_error(s: &str) -> String {
    // string.split("") always returns at least two elements containing line-break-chars, hence why -2
    format!("{}/{}", s.split("").filter(|&letter| letter < "a" || letter > "m").count() - 2, s.len())
}

#[test]
fn test1() {
    assert_eq!(printer_error("aaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbmmmmmmmmmmmmmmmmmmmxyz"), "3/56")
}

#[test]
fn test2() {
    assert_eq!(printer_error(""), "0/0")
}

#[test]
fn test3() {
    assert_eq!(printer_error("aaabbbbhaijjjm"), "0/14")
}

#[test]
fn test4() {
    assert_eq!(printer_error("kkkwwwaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbmmmmmmmmmmmmmmmmmmmxyz"), "6/60");
}

#[test]
fn test5() {
    assert_eq!(printer_error("kkkwwwaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbmmmmmmmmmmmmmmmmmmmxyzuuuuu"), "11/65");
}

