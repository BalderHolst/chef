use rand::prelude::*;

const ADVICE: &'static [&'static str] = &[
    "Are you following a recipe?",
    "Have you sharpened your knives recently? That may help!",
    "Maybe add some garlic powder?",
    "I think it needs more salt.",
    "You could always just order something...",
    "It needs a little spice.",
    "Not quite done.",
    "Overcooked.",
    "Uff.",
    "Are you sure those are the right proportions?",
    "Measure twice, mix once.",
    "Have you remembered to preheat the oven?",
    "...is that how its supposed to look?",
    "Experimentation is good!",
    "If you mix cream too long, it'll turn into butter!",
    "A good cook knows their spices!",
    "That is... unique.",
    "Did you read the entire recipe?",
    "Its good to clean as you go.",
];

/// Prints a random line of cooking advice to the terminal
pub fn give_advice() {
    let index: usize = rand::thread_rng().gen_range(0..ADVICE.len());
    let advice = ADVICE[index];
    println!("{advice}");
}
