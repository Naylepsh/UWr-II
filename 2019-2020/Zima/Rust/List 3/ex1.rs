fn row_sum_odd_numbers(n:i64) -> i64 {
  return n * n * n
}

#[test]
fn test0() {
        assert_eq!(row_sum_odd_numbers(4), 64)
}
#[test]
fn test1() {
        assert_eq!(row_sum_odd_numbers(20), 8000)
}
#[test]
fn test2() {
        assert_eq!(row_sum_odd_numbers(11), 1331)
}
#[test]
fn test3() {
        assert_eq!(row_sum_odd_numbers(17), 4913)
}
#[test]
fn test4() {
        assert_eq!(row_sum_odd_numbers(9), 729)
}
#[test]
fn test5() {
        assert_eq!(row_sum_odd_numbers(19), 6859)
}
#[test]
fn test6() {
        assert_eq!(row_sum_odd_numbers(3), 27)
}
#[test]
fn test7() {
        assert_eq!(row_sum_odd_numbers(11), 1331)
}
#[test]
fn test8() {
        assert_eq!(row_sum_odd_numbers(1), 1)
}
#[test]
fn test9() {
        assert_eq!(row_sum_odd_numbers(4), 64)
}

