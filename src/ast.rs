mod types;
pub use types::*;

// parse identifier or identifier with data
pub fn parse_identifier(input: &str) -> (String, Option<String>) {
    let parts = input.split('(').collect::<Vec<_>>();
    // SOME_IDENTIFIER(2)
    // -> ["SOME_IDENTIFIER", "2)"]

    let name = parts[0].to_string();

    if let Some(data) = parts.get(1) {
        let data = data.chars().take_while(|&x| x != ')').collect::<String>();
        (name, Some(data))
    } else {
        (name, None)
    }
}

pub fn parse() {
    let symbols = std::fs::read_to_string("locals.symbols").unwrap();

    let ast = symbols.split(" == BOUND SYNTAX TREE ==\n").last().unwrap();
    let ast = ast.split("\n").map(|x| x.trim_end()).collect::<Vec<&str>>();

    for line in ast {
        let indent = line.chars().take_while(|x| *x == ' ').count();

        let remaining = line.chars().skip(indent).collect::<String>();

        let objects = remaining.split_terminator(' ').collect::<Vec<_>>();
        let objects = objects
            .into_iter()
            .map(parse_identifier)
            .collect::<Vec<_>>();

        dbg!(&objects);
    }
}
