mod driver;
mod parser;
mod scanner;
mod interpreter;

use driver::Driver;
fn main() {
    let mut d = Driver::new();
    d.main_loop();
}
