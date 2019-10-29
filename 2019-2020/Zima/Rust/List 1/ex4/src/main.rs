fn square_area_to_circle(size:f64) -> f64 {
    std::f64::consts::PI * size / 4.0
}

fn assert_close(a:f64, b:f64, epsilon:f64) {
    assert!( (a-b).abs() < epsilon, format!("Expected: {}, got: {}",b,a) );
}

#[test]
fn tet1() {
  assert_close(square_area_to_circle(9.0), 7.0685834705770345, 1e-8);
}

#[test]
fn test2() {
    assert_close(square_area_to_circle(20.0), 15.70796326794897, 1e-8);
}

#[test]
fn test3() {
    assert_close(square_area_to_circle(25.0), 19.63495408493620, 1e-8);
}

#[test]
fn test4() {
    assert_close(square_area_to_circle(36.0), 28.27433388230813, 1e-8);
}

#[test]
fn test5() {
    assert_close(square_area_to_circle(49.0), 38.48451000647496, 1e-8);
}