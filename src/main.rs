use std::io::BufWriter;

mod ast;

fn main() {
    let v = ast::parse();

    let stdout = std::io::stdout();
    let writer = BufWriter::new(stdout.lock());

    v.compile(writer);
}
