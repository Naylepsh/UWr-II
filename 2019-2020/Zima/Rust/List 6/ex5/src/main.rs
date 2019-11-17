struct Sudoku{
  data: Vec<Vec<u32>>,
}


impl Sudoku{
  fn is_valid(&self) -> bool {
    let n = self.data.len();
    let proper: Vec<u32> = (1..=(n as u32)).collect();
    // rows vals check
    for row in &self.data {
        let mut x = row.clone();
        x.sort();
        if x != proper {
            return false;
        }
    }
    // cols vals check
    for i in 0..n {
        let mut x = self.data.clone().into_iter().map(|vec| vec[i]).collect::<Vec<u32>>();
        x.sort();
        if x != proper {
            return false;
        }
    }
    // squares check
    let m = (n as f64).sqrt() as usize;
    for i in (0..n).step_by(m) {
        for j in (0..n).step_by(m) {
            let mut square_vals = vec![];
            for y_off in 0..m {
                for x_off in 0..m {
                    if i+y_off >= n || j+x_off >= n {
                        return false;
                    }
                    square_vals.push(self.data[i+y_off][j+x_off]);
                }
            }
            square_vals.sort();
            if square_vals != proper {
                return false;
            }
        }
    }
    true
  }
}

#[test]
fn test1() {
  let good_sudoku_1 = Sudoku{
    data: vec![
            vec![7,8,4, 1,5,9, 3,2,6],
            vec![5,3,9, 6,7,2, 8,4,1],
            vec![6,1,2, 4,3,8, 7,5,9],

            vec![9,2,8, 7,1,5, 4,6,3],
            vec![3,5,7, 8,4,6, 1,9,2],
            vec![4,6,1, 9,2,3, 5,8,7],
            
            vec![8,7,6, 3,9,4, 2,1,5],
            vec![2,4,3, 5,6,1, 9,7,8],
            vec![1,9,5, 2,8,7, 6,3,4]
        ]
  };
  assert!(good_sudoku_1.is_valid());
}

#[test]
fn test2() {
  let good_sudoku_2 = Sudoku{
    data: vec![
            vec![1, 4,  2, 3],
            vec![3, 2,  4, 1],
    
            vec![4, 1,  3, 2],
            vec![2, 3,  1, 4],
        ]
  };
  assert!(good_sudoku_2.is_valid());
}

#[test]
fn test3() {
  let bad_sudoku_1 = Sudoku{
    data: vec![
            vec![1,2,3, 4,5,6, 7,8,9],
            vec![1,2,3, 4,5,6, 7,8,9],
            vec![1,2,3, 4,5,6, 7,8,9],

            vec![1,2,3, 4,5,6, 7,8,9],
            vec![1,2,3, 4,5,6, 7,8,9],
            vec![1,2,3, 4,5,6, 7,8,9],
            
            vec![1,2,3, 4,5,6, 7,8,9],
            vec![1,2,3, 4,5,6, 7,8,9],
            vec![1,2,3, 4,5,6, 7,8,9],
        ]
  };
  assert!(!bad_sudoku_1.is_valid());
}

#[test]
fn test4() {
    let bad_sudoku_2 = Sudoku{
        data: vec![
                vec![1,2,3,4,5],
                vec![1,2,3,4],
                vec![1,2,3,4],
                vec![1],
            ]
    };
    assert!(!bad_sudoku_2.is_valid());
}

#[test]
fn test5() {
    let good_sudoku = Sudoku{
        data: vec![vec![1]]
    };
    assert!(good_sudoku.is_valid());
}

#[test]
fn test6() {
    let good_sudoku =  Sudoku{
        data: vec![
            vec![4,1,2,3],
            vec![2,3,4,1],
            vec![3,4,1,2],
            vec![1,2,3,4]
        ]
    };
    assert!(good_sudoku.is_valid());
}

#[test]
fn test7() {
    let bad_sudoku = Sudoku{
        data: vec![
            vec![1,3],
            vec![2,4]
        ]
    };
    assert!(!bad_sudoku.is_valid());
}

#[test]
fn test8() {
    let good_sudoku = Sudoku{
        data: vec![
            vec![1,2,3,4],
            vec![4,3,2,1],
            vec![3,4,1,2],
            vec![2,1,4,3]
        ]
    };
    assert!(good_sudoku.is_valid());
}

#[test]
fn test9() {
    let bad_sudoku = Sudoku{
        data: vec![
            vec![1,2,3,4,5,6,7,8,9],
            vec![9,1,2,3,4,5,6,7,8],
            vec![8,9,1,2,3,4,5,6,7],
            vec![7,8,9,1,2,3,4,5,6],
            vec![6,7,8,9,1,2,3,4,5],
            vec![5,6,7,8,9,1,2,3,4],
            vec![4,5,6,7,8,9,1,2,3],
            vec![3,4,5,6,7,8,9,1,2],
            vec![2,3,4,5,6,7,8,9,1],
            vec![1,2,3,4,5,6,7,8,9]
        ]
    };
    assert!(!bad_sudoku.is_valid());
}

#[test]
fn test10() {
    let sudoku = Sudoku{
        data: vec![
            vec![1,4,3,2],
            vec![1,4,3,2],
            vec![3,2,4,1],
            vec![4,3,2,1]
        ]
    };
    assert!(!sudoku.is_valid());
}