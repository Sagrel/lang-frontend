use std::{env, thread};

use lang::*;

fn main() {
    let path = if let Some(path) = env::args().nth(1) {
        path
    } else {
        "./examples/simple".to_owned()
    };

    let builder = thread::Builder::new()
        .name("compilando con mucha recursion".into())
        .stack_size(32 * 1024 * 1024); // 32MB of stack space

    let handler = builder.spawn(|| compile(path)).unwrap();

    handler.join().unwrap();
}
