pub fn main() {
    let elves: Vec<&str> = include_str!("../../data/input1.txt")
        .split("\n\n")
        .collect::<Vec<&str>>();
    let calories: Vec<u32> = elves
        .iter()
        .map(|elf| {
            elf.lines()
                .map(|calorie| calorie.parse::<u32>().unwrap())
                .sum()
        })
        .collect::<Vec<u32>>();
    let part1: &u32 = calories.iter().max().unwrap();
    // surely it isn't me using rust non-idiomatically ;)
    let mut rust_is_stupid: Vec<u32> = calories.to_vec();
    rust_is_stupid.sort();
    let part2: u32 = rust_is_stupid.as_slice()[rust_is_stupid.len()-3..].iter().sum::<u32>();
    println!("{:?}", part1);
    println!("{:?}", part2);
}
