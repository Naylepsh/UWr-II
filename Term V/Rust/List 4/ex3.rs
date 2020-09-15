fn main() {
    println!("{}", highlight("FRLFRLFRLLLLL"));
}

pub fn highlight(code: &str) -> String {
  my_split(code).iter().fold("".to_string(), |prev, cmd| prev+&colorise(cmd))
}

fn my_split(string: &str) -> Vec<String> {
  let chars = string.chars();
  let mut _prev = ' ';
  let mut _strings = Vec::new();
  let mut _current = String::new();
  for char in chars {
    if char == _prev || (char.is_digit(10) && _prev.is_digit(10)){
      _current += &char.to_string();
    } else {
      if _current.len() > 0 {
          _strings.push(_current);
      }
      _current = String::from(char.to_string());
      _prev = char;
    }
  }
  _strings.push(_current);
  _strings
}

fn colorise(cmd: &str) -> String {
  let mut _color: String;
  let chr = cmd.chars().collect::<Vec<char>>()[0];
  if chr == 'F' {
    _color = "pink".to_string();
  } else if chr == 'L' {
    _color = "red".to_string();
  } else if chr == 'R' {
    _color = "green".to_string();
  } else if chr.is_digit(10) { 
    _color = "orange".to_string();
  } else {
    return format!("{}", cmd);
  }
  return format!("{}{}{}{}{}", "<span style=\"color: ", _color, "\">", cmd, "</span>");
}

#[test]
fn test1() {
    assert_eq!(highlight("1234F1234"), "<span style=\"color: orange\">1234</span><span style=\"color: pink\">F</span><span style=\"color: orange\">1234</span>")
}

#[test]
fn test2() {
    assert_eq!(highlight("FFFF1212"), "<span style=\"color: pink\">FFFF</span><span style=\"color: orange\">1212</span>");
}

#[test]
fn test3() {
    assert_eq!(highlight("FRFRFR11"), "<span style=\"color: pink\">F</span><span style=\"color: green\">R</span><span style=\"color: pink\">F</span><span style=\"color: green\">R</span><span style=\"color: pink\">F</span><span style=\"color: green\">R</span><span style=\"color: orange\">11</span>");
}

#[test]
fn test4() {
    assert_eq!(highlight("FRLFRLFRLLLLL"), r#"<span style="color: pink">F</span><span style="color: green">R</span><span style="color: red">L</span><span style="color: pink">F</span><span style="color: green">R</span><span style="color: red">L</span><span style="color: pink">F</span><span style="color: green">R</span><span style="color: red">LLLLL</span>"#);
}

#[test]
fn test5() {
    assert_eq!(highlight("F(FF)"), r#"<span style="color: pink">F</span>(<span style="color: pink">FF</span>)"#);
}

#[test]
fn test6() {
    assert_eq!(highlight("F(FF)(RR)1200101RR"), r#"<span style="color: pink">F</span>(<span style="color: pink">FF</span>)(<span style="color: green">RR</span>)<span style="color: orange">1200101</span><span style="color: green">RR</span>"#);
}

#[test]
fn test7() {
    assert_eq!(highlight("()()()"), "()()()");
}

#[test]
fn test8() {
    assert_eq!(highlight("(11)(22)(33)"), r#"(<span style="color: orange">11</span>)(<span style="color: orange">22</span>)(<span style="color: orange">33</span>)"#);
}

#[test]
fn test9() {
    assert_eq!(highlight("(11)(22)(33)(44)"), r#"(<span style="color: orange">11</span>)(<span style="color: orange">22</span>)(<span style="color: orange">33</span>)(<span style="color: orange">44</span>)"#);
}

#[test]
fn test10() {
    assert_eq!(highlight("1"), r#"<span style="color: orange">1</span>"#);
}