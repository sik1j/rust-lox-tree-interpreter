mod driver;
mod parser;
mod scanner;
mod interpreter;
mod environment;

use driver::Driver;
fn main() {
    let mut d = Driver::new();
    d.main_loop();
}
