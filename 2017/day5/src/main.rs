use std::fs::File;
use std::io::BufReader;
use std::io::prelude::*;

fn main() {
    let file = File::open("input").unwrap();
    let reader = BufReader::new(file);
    let mut jumps: Vec<i32> = reader.lines()
        .map(|num| num.unwrap().parse::<i32>().unwrap())
        .collect();
    let mut jumps2 = jumps.clone();
    
    let steps1 = count_steps_1(&mut jumps);
    println!("Puzzle 1: The amount of steps required to exit the maze is: {}", steps1);

    let steps2 = count_steps_2(&mut jumps2);
    println!("Puzzle 2: The amount of steps required to exit the maze is: {}", steps2);

}

fn count_steps_1(jumps: &mut Vec<i32>) -> u32 {
    let mut steps: u32 = 0;
    let mut cur_ind: i32 = 0;

    while let Some(cur_val) = jumps.get_mut(cur_ind as usize) {
        cur_ind += *cur_val;
        *cur_val += 1;
        steps += 1;
    }
    
    steps
}

fn count_steps_2(jumps: &mut Vec<i32>) -> u32 {
    let mut steps: u32 = 0;
    let mut cur_ind: i32 = 0;

    while let Some(cur_val) = jumps.get_mut(cur_ind as usize) {
        cur_ind += *cur_val;
        if *cur_val >= 3 {
            *cur_val -= 1;
        } else {
            *cur_val += 1;
        }
        steps += 1;
    }
    
    steps
}

