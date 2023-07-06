//! Responsible for giving cooking advice to the user.

use rand::prelude::*;

/// A newline seperated list of cooking advice!
const ADVICE: &str = include_str!("advice.txt");

/// Prints a random line of cooking advice to the terminal
pub fn give_advice() {
    let index: usize = rand::thread_rng().gen_range(0..ADVICE.lines().count());
    let advice = ADVICE.lines().nth(index).unwrap();
    println!("{advice}");
}
