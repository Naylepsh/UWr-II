use std::collections::BTreeMap;

fn letter_frequency(input: &str) -> BTreeMap<char, i32> {
    let mut freq: BTreeMap<char, i32> = BTreeMap::new();
    
    for c in input.to_lowercase().chars().filter(|c| c.is_alphabetic()) {
        *freq.entry(c).or_insert(0) += 1;
    }
    freq
}

#[test]
fn simpleword() {
    let answer: BTreeMap<char, i32> =
    [('a', 2),
        ('c', 1),
        ('l', 1),
        ('t', 1),
        ('u', 1)]
        .iter().cloned().collect();
        
    assert_eq!(letter_frequency("actual"), answer);
}

#[test]
fn sequence() {
    let answer: BTreeMap<char, i32> =
    [('a', 3),
        ('b', 2),
        ('f', 1),
        ('p', 1),
        ('s', 1),
        ('t', 2),
        ('u', 1),
        ('x', 5)]
        .iter().cloned().collect();
        
    assert_eq!(letter_frequency("AaabBF UttsP xxxxx"), answer);
}

#[test]
fn test3() {
    // {'a': 4, 'd': 1, 'f': 2, 's': 2, 'x': 3}
    let answer: BTreeMap<char, i32> =
    [('a', 4),
     ('d', 1), 
     ('f', 2), 
     ('s', 2), 
     ('x', 3)]
        .iter().cloned().collect();
        
    assert_eq!(letter_frequency("XXx 7.% asdFFSAaa"), answer);
}

#[test]
fn test4() {
    let answer: BTreeMap<char, i32> =
    [('a', 5), 
    ('c', 1), 
    ('d', 2), 
    ('e', 5), 
    ('f', 1), 
    ('g', 4), 
    ('h', 1), 
    ('i', 7), 
    ('k', 1), 
    ('l', 2), 
    ('m', 3), 
    ('n', 5), 
    ('o', 3), 
    ('r', 2), 
    ('s', 4), 
    ('t', 3), 
    ('u', 1),
    ('y', 1)]
        .iter().cloned().collect();
        
    assert_eq!(letter_frequency("As long as I\'m learning something, I figure I\'m OK - it\'s a decent day."), answer);
}

#[test]
fn test5() {
    let answer: BTreeMap<char, i32> =
    [('a', 3), 
    ('b', 2), ('f', 1), ('p', 1), ('s', 1), ('t', 2), ('u', 1), ('x', 5)]
        .iter().cloned().collect();
    
    assert_eq!(letter_frequency("AaabBF UttsP xxxxx"), answer);
}

#[test]
fn test6() {
    let answer: BTreeMap<char, i32> =
    [].iter().cloned().collect();
    
    assert_eq!(letter_frequency(""), answer);
}

#[test]
fn test7() {
    let answer: BTreeMap<char, i32> =
    [].iter().cloned().collect();
    
    assert_eq!(letter_frequency("7777!@!@!"), answer);
}

#[test]
fn test8() {
    let answer: BTreeMap<char, i32> =
    [('h' , 3), ('a' , 3)].iter().cloned().collect();
    
    assert_eq!(letter_frequency("hahaha"), answer);
}

#[test]
fn test9() {
    let answer: BTreeMap<char, i32> =
    [('h' , 3), ('a' , 3)].iter().cloned().collect();
    
    assert_eq!(letter_frequency("ha ha ha"), answer);
}

#[test]
fn test10() {
    let answer: BTreeMap<char, i32> =
    [('h', 6), ('a', 3), ('o', 3)].iter().cloned().collect();
    
    assert_eq!(letter_frequency("hahaha ho ho ho"), answer);
}
