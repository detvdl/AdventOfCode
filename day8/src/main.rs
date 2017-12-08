#![feature(slice_patterns)]

use std::collections::HashMap;
use std::fs::File;
use std::io::BufReader;
use std::io::prelude::*;

fn main() {
    let file = File::open("input").unwrap();
    let reader = BufReader::new(file);
    let mut registers: HashMap<String, isize> = HashMap::new();
    let mut max = 0;
    for line in reader.lines() {
        let mut instr = line.unwrap();
        let mut instr: Vec<&str> = instr.split(" ").collect();
        if let &[op_lval, op, op_rval, "if", cond_l, cond, cond_r] = &instr[..] {
            let &cond_l: &isize = registers.entry(cond_l.to_owned()).or_insert(0);
            let op_r: isize = op_rval.parse::<isize>().unwrap();
            let cond_r: isize = cond_r.parse::<isize>().unwrap();

            let condition: bool = match &cond[..] {
                "<" => cond_l < cond_r,
                "<=" => cond_l <= cond_r,
                "==" => cond_l == cond_r,
                "!=" => cond_l != cond_r,
                ">" => cond_l > cond_r,
                ">=" => cond_l >= cond_r,
                _ => false,
            };
            
            if condition {
                let mut op_l: &mut isize = registers.entry(op_lval.to_owned()).or_insert(0);
                match &op[..] {
                    "dec" => *op_l -= op_r,
                    "inc" => *op_l += op_r,
                    _ => println!("Unsupported operation: {}", op),
                }
                if *op_l > max {
                    max = *op_l;
                }
            }
        }
    }
    println!("Max value at end: {}", registers.values().max().unwrap_or(&0));
    println!("Max value while running: {}", max);
}
