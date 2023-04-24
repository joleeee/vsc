mod types;
use std::collections::VecDeque;

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

    let ast = symbols.split(" == BOUND SYNTAX TREE == \n").last().unwrap();
    let ast = ast.split("\n").map(|x| x.trim_end()).collect::<Vec<&str>>();

    let mut symbols = VecDeque::new();

    for line in ast {
        let indent = line.chars().take_while(|x| *x == ' ').count();

        let remaining = line.chars().skip(indent).collect::<String>();

        let objects = remaining.split_terminator(' ').collect::<Vec<_>>();
        let objects = objects
            .into_iter()
            .map(parse_identifier)
            .collect::<Vec<_>>();

        symbols.push_back(Parent { indent, objects })
    }

    #[derive(Debug)]
    struct Parent {
        indent: usize,
        objects: Vec<(String, Option<String>)>,
    }

    let mut parents: VecDeque<&Parent> = VecDeque::new();

    let global_list = symbols.pop_front().unwrap();
    dbg!(&global_list);
    assert_eq!(global_list.objects[0].0, "GLOBAL_LIST");
    parents.push_back(&global_list);

    for sym in &symbols {
        while sym.indent < parents.back().unwrap().indent {
            let mut arguments = vec![];

            // we are at a lower indentation level, pop parents until we are at the same level
            let target_indentation = parents.back().unwrap().indent - 1;

            while target_indentation < parents.back().unwrap().indent {
                arguments.push(parents.pop_back().unwrap());
            }

            println!(
                "> Calling {} with {} arguments",
                parents.back().unwrap().objects[0].0,
                arguments.len()
            );
            for a in arguments {
                println!("> > {} {:?}", a.objects[0].0, a.objects[0].1);
            }
        }

        parents.push_back(sym);

        println!("{}{:?}", " ".repeat(sym.indent), sym.objects);
    }
}
