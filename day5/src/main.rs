use std::fs::File;
use std::io::BufReader;
use std::io::prelude::*;

fn main() {
    let file = File::open("input").unwrap();
    let mut bufread = BufReader::new(file);
    let mut contents = String::new();
    bufread.read_to_string(&mut contents).unwrap();

    let jumps1 = parse_input(contents);
    let start = 0;
    let jumps2 = jumps1.clone();
    
    let steps1 = count_steps_1(start, jumps1);
    println!("Puzzle 1: The amount of steps required to exit the maze is: {}", steps1);

    let steps2 = count_steps_2(start, jumps2);
    println!("Puzzle 2: The amount of steps required to exit the maze is: {}", steps2);

}

fn parse_input(contents: String) -> Vec<i32> {
    let mut parsed = Vec::new();
    let split = contents.split("\n");
    for step in split {
        let val = step.parse();
        match val {
            Ok(v) => parsed.push(v),
            Err(_) => {}
        }
    }
    parsed
}

fn count_steps_1(start: usize, mut jumps: Vec<i32>) -> u32 {
    let mut steps: u32 = 0;
    let mut cur_ind = start as i32;

    while let Some(cur_val) = jumps.get_mut(cur_ind as usize) {
        cur_ind += *cur_val;
        *cur_val += 1;
        steps += 1;
    }
    
    steps
}

fn count_steps_2(start: usize, mut jumps: Vec<i32>) -> u32 {
    let mut steps: u32 = 0;
    let mut cur_ind = start as i32;

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

