use std::iter::zip;
use std::fs;

fn read_input() -> String {
    fs::read_to_string("2021/01/input.txt").expect("couldn't read input file")
}

fn parse_input(input: String) -> Vec<i32> {
    input.lines().map(|l| l.parse().unwrap()).collect()
}

fn count_inc(vec: &Vec<i32>) -> usize {
    zip(&vec[1..], &*vec)
        .filter_map(|(a, b)| (a > b).then_some(1))
        .count()
}

fn sliding_windows(vec: &Vec<i32>) -> Vec<i32> {
    zip(&vec[2..], &vec[1..])
        .zip(&*vec)
        .map(|((a, b), c)| a + b + c)
        .collect()
}

pub fn part1(input_vec: &Vec<i32>) -> usize {
    count_inc(input_vec)
}

pub fn part2(input_vec: &Vec<i32>) -> usize {
    count_inc(&sliding_windows(input_vec))
}

pub fn run() {
    let input = read_input();
    let input_vec = parse_input(input);
    println!("answers\npart1: {}\npart2: {}", part1(&input_vec), part2(&input_vec))
}

#[cfg(test)]
mod test {
    use super::*;
    use rstest::*;

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
        assert_eq!(part1(&parse_input(example)), example_out)
    }
}
