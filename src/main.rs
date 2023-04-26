use std::io::BufWriter;

mod ast;

fn main() {
    let v = ast::parse();
    println!("{:#?}", v.root);

    let out = std::fs::File::create("out.s").unwrap();

    let writer = BufWriter::new(out);
    //let stdout = std::io::stdout();
    //let stdout = BufWriter::new(stdout.lock());

    v.compile(writer);
}
