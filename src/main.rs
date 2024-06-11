mod driver;
mod parser;
mod scanner;

use driver::Driver;
fn main() {
    let mut d = Driver::new();
    d.main_loop();
}
