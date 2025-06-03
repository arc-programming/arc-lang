// Test file for module system features
mod math;

use std::io;
use collections::Vec;

pub fn main() {
    println("Hello, world!");
}

pub mod utils {
    pub fn helper() {
        return 42;
    }
}
