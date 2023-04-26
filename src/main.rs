mod ast;

fn main() {
    println!("Hello, world!");

    let v = ast::parse();
    dbg!(&v.root);

    v.root.compile();
}
