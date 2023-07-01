use rand::prelude::*;

const ADVICE: &str = include_str!("advice.txt");

/// Prints a random line of cooking advice to the terminal
pub fn give_advice() {
    let index: usize = rand::thread_rng().gen_range(0..ADVICE.lines().count());
    let advice = ADVICE.lines().nth(index).unwrap();
    println!("{advice}");
}
