use std::iter::zip;
use std::fs;

fn read_input() -> String {
    fs::read_to_string("2021/01/input.txt").expect("couldn't read input file")
}

fn parse_input(input: String) -> Vec<i32> {
    input.lines().map(|l| l.parse().unwrap()).collect()
}

fn count_inc(vec: Vec<i32>) -> usize {
    zip(&vec[1..], &vec)
        .filter_map(|(a, b)| (a > b).then_some(1))
        .count()
}

pub fn part1() -> usize {
    let input = read_input();
    input = parse_input(input);
    count_inc(input)
}

pub fn main() {
    println!("answer : {:?}", part1())
}

#[cfg(test)]
mod test {
    use super::*;
    use rstest::*;

    #[fixture]
    fn input() -> String {
        fs::read_to_string("2021/01/input.txt").expect("couldn't read input file")
    }

    #[fixture]
    fn example() -> String {
        fs::read_to_string("2021/01/example.txt").expect("couldn't read input file")
    }

    #[fixture]
    fn example_out() -> usize {
        7
    }

    #[rstest]
    fn test_example(example: String, example_out: usize) {
        assert_eq!(part1(example), example_out)
    }

    #[rstest]
    fn test_input(input: String) {
        let out = part1(input);
        println!("{:?}", out);
    }
}
