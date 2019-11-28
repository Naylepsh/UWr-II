fn main() {
    next_bigger_number(19);
}

fn next_bigger_number(n: i64) -> i64 {
    let mut s = n.to_string().chars().collect::<Vec<char>>();
    s.sort();
    let a: String = s.iter().rev().collect();
    let m = a.parse::<i64>().unwrap();
    if n == m {
        return -1;
    }
    return m;
}
