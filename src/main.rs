mod ast;

fn main() {
    println!("Hello, world!");

    let v = ast::parse();
    dbg!(&v);

    v.compile();
}
