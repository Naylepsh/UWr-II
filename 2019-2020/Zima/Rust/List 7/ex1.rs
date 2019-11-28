fn chessboard_cell_color(cell1: &str, cell2: &str) -> bool {    
    get_ascii_value(cell1) % 2 == get_ascii_value(cell2) % 2
}

fn get_ascii_value(cell: &str) -> u8 {
    cell.chars().map(|c| c as u8).sum()
}

#[test]
fn test1() {
    assert_eq!(chessboard_cell_color("A1", "C3"), true);
}

#[test]
fn test2() {
    assert_eq!(chessboard_cell_color("A1", "H3"), false);
}

#[test]
fn test3() {
    assert_eq!(chessboard_cell_color("A1", "A2"), false);
}

#[test]
fn test4() {
    assert_eq!(chessboard_cell_color("A1", "C1"), true);
}

#[test]
fn test5() {
    assert_eq!(chessboard_cell_color("A1", "A1"), true);
}

#[test]
fn test6() {
    assert_eq!(chessboard_cell_color("A5", "B1"), false);
}

#[test]
fn test7() {
    assert_eq!(chessboard_cell_color("D8", "G6"), false);
}

#[test]
fn test8() {
    assert_eq!(chessboard_cell_color("D1", "E4"), true);
}

#[test]
fn test9() {
    assert_eq!(chessboard_cell_color("B7", "H6"), false);
}

#[test]
fn test10() {
    assert_eq!(chessboard_cell_color("C2", "B4"), false);
}
