fn zoom(n: i32) -> String {
    let cols = n as usize;
    let rows = n as usize;
    let outer = if ((n+1) / 2) % 2 != 0 { "■" } else { "□" };
    let inner = if ((n+1) / 2) % 2 == 0 { "■" } else { "□" };
    let mut square = vec![vec!["■"; cols]; rows];
    for i in 0..cols {
        let symbol = if i % 2 == 0 { outer } else { inner };
        for j in i..(cols-i) {
            square[       i][       j]= symbol;
            square[cols-i-1][       j] = symbol;
            square[       j][       i] = symbol;
            square[       j][cols-i-1] = symbol;
        }
    }
    format!("{}", square.into_iter().map(|v| v.into_iter().collect::<String>()).collect::<Vec<_>>().join("\n"))
}


#[test]
fn basic_test_1() {
  assert_eq!(zoom(1), "■");
}

#[test]
fn basic_test_2() {
  assert_eq!(zoom(3), "\
□□□
□■□
□□□"
  );
}

#[test]
fn basic_test_3() {
  assert_eq!(zoom(5), "\
■■■■■
■□□□■
■□■□■
■□□□■
■■■■■"
  );
}

#[test]
fn basic_test_4() {
  assert_eq!(zoom(7), "\
□□□□□□□
□■■■■■□
□■□□□■□
□■□■□■□
□■□□□■□
□■■■■■□
□□□□□□□"
  );
}

#[test]
fn basic_test_5() {
  assert_eq!(zoom(9), "\
■■■■■■■■■
■□□□□□□□■
■□■■■■■□■
■□■□□□■□■
■□■□■□■□■
■□■□□□■□■
■□■■■■■□■
■□□□□□□□■
■■■■■■■■■"
  );
}

#[test]
fn basic_test_6() {
  assert_eq!(zoom(11), "\
□□□□□□□□□□□
□■■■■■■■■■□
□■□□□□□□□■□
□■□■■■■■□■□
□■□■□□□■□■□
□■□■□■□■□■□
□■□■□□□■□■□
□■□■■■■■□■□
□■□□□□□□□■□
□■■■■■■■■■□
□□□□□□□□□□□"
  );
}

#[test]
fn basic_test_7() {
  assert_eq!(zoom(13), "\
■■■■■■■■■■■■■
■□□□□□□□□□□□■
■□■■■■■■■■■□■
■□■□□□□□□□■□■
■□■□■■■■■□■□■
■□■□■□□□■□■□■
■□■□■□■□■□■□■
■□■□■□□□■□■□■
■□■□■■■■■□■□■
■□■□□□□□□□■□■
■□■■■■■■■■■□■
■□□□□□□□□□□□■
■■■■■■■■■■■■■"
  );
}

#[test]
fn basic_test_8() {
  assert_eq!(zoom(17), "\
■■■■■■■■■■■■■■■■■
■□□□□□□□□□□□□□□□■
■□■■■■■■■■■■■■■□■
■□■□□□□□□□□□□□■□■
■□■□■■■■■■■■■□■□■
■□■□■□□□□□□□■□■□■
■□■□■□■■■■■□■□■□■
■□■□■□■□□□■□■□■□■
■□■□■□■□■□■□■□■□■
■□■□■□■□□□■□■□■□■
■□■□■□■■■■■□■□■□■
■□■□■□□□□□□□■□■□■
■□■□■■■■■■■■■□■□■
■□■□□□□□□□□□□□■□■
■□■■■■■■■■■■■■■□■
■□□□□□□□□□□□□□□□■
■■■■■■■■■■■■■■■■■"
  );
}

#[test]
fn basic_test_9() {
  assert_eq!(zoom(19), "\
□□□□□□□□□□□□□□□□□□□
□■■■■■■■■■■■■■■■■■□
□■□□□□□□□□□□□□□□□■□
□■□■■■■■■■■■■■■■□■□
□■□■□□□□□□□□□□□■□■□
□■□■□■■■■■■■■■□■□■□
□■□■□■□□□□□□□■□■□■□
□■□■□■□■■■■■□■□■□■□
□■□■□■□■□□□■□■□■□■□
□■□■□■□■□■□■□■□■□■□
□■□■□■□■□□□■□■□■□■□
□■□■□■□■■■■■□■□■□■□
□■□■□■□□□□□□□■□■□■□
□■□■□■■■■■■■■■□■□■□
□■□■□□□□□□□□□□□■□■□
□■□■■■■■■■■■■■■■□■□
□■□□□□□□□□□□□□□□□■□
□■■■■■■■■■■■■■■■■■□
□□□□□□□□□□□□□□□□□□□"
  );
}

#[test]
fn basic_test_10() {
  assert_eq!(zoom(21), "\
■■■■■■■■■■■■■■■■■■■■■
■□□□□□□□□□□□□□□□□□□□■
■□■■■■■■■■■■■■■■■■■□■
■□■□□□□□□□□□□□□□□□■□■
■□■□■■■■■■■■■■■■■□■□■
■□■□■□□□□□□□□□□□■□■□■
■□■□■□■■■■■■■■■□■□■□■
■□■□■□■□□□□□□□■□■□■□■
■□■□■□■□■■■■■□■□■□■□■
■□■□■□■□■□□□■□■□■□■□■
■□■□■□■□■□■□■□■□■□■□■
■□■□■□■□■□□□■□■□■□■□■
■□■□■□■□■■■■■□■□■□■□■
■□■□■□■□□□□□□□■□■□■□■
■□■□■□■■■■■■■■■□■□■□■
■□■□■□□□□□□□□□□□■□■□■
■□■□■■■■■■■■■■■■■□■□■
■□■□□□□□□□□□□□□□□□■□■
■□■■■■■■■■■■■■■■■■■□■
■□□□□□□□□□□□□□□□□□□□■
■■■■■■■■■■■■■■■■■■■■■"
  );
}