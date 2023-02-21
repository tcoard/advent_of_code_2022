use itertools::sorted;
use itertools::Itertools;

fn get_first_marker(raw_message: &str, message_len: usize) -> usize {
    raw_message
        .chars()
        .collect::<Vec<char>>()
        .windows(message_len)
        .map(|x| sorted(x).dedup().count())
        .take_while(|x| x < &message_len)
        .count()
        + message_len
}

pub fn main() {
    let raw_message: &str = include_str!("../../data.txt");
    let part1 = get_first_marker(raw_message, 4);
    let part2 = get_first_marker(raw_message, 14);
    println!("{}", part1);
    println!("{}", part2);
}
