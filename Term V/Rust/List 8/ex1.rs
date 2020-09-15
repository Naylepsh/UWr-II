fn main() {
    println!("Hello, world!");
}

fn dna_strand(dna: &str) -> String {
    dna.chars().map( |c| match c {
        'A' => 'T',
        'T' => 'A',
        'C' => 'G',
        'G' => 'C',
        other_char => other_char
    }).collect()
}

#[test]
fn test1() {
    assert_eq!(dna_strand("AAAA"),"TTTT");
}

#[test]
fn test2() {
    assert_eq!(dna_strand("ATTGC"),"TAACG");
}

#[test]
fn test3() {
    assert_eq!(dna_strand("GTAT"),"CATA");
}

#[test]
fn test4() {
    assert_eq!(dna_strand("GTATA"),"CATAT");
}

#[test]
fn test5() {
    assert_eq!(dna_strand("ATTA"),"TAAT");
}

#[test]
fn test6() {
    assert_eq!(dna_strand("TTT"),"AAA");
}

#[test]
fn test7() {
    assert_eq!(dna_strand("GG"),"CC");
}

#[test]
fn test8() {
    assert_eq!(dna_strand("AGTC"),"TCAG");
}

#[test]
fn test9() {
    assert_eq!(dna_strand("ACC"),"TGG");
}

#[test]
fn test10() {
    assert_eq!(dna_strand("ATGCATGC"),"TACGTACG");
}