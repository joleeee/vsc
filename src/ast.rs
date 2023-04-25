mod types;
use std::collections::VecDeque;

pub use types::*;

// parse identifier or identifier with data
pub fn parse_identifier(input: &str) -> Field {
    let parts = input.split('(').collect::<Vec<_>>();
    // SOME_IDENTIFIER(2)
    // -> ["SOME_IDENTIFIER", "2)"]

    let name = parts[0].to_string();

    let argument = parts
        .get(1)
        .and_then(|data| Some(data.chars().take_while(|&x| x != ')').collect::<String>()));

    Field { name, argument }
}

#[derive(Debug, Clone)]
pub struct Entry(Vec<Field>);

#[derive(Debug, Clone)]
struct RawSymbol {
    indent: usize,
    objects: Vec<Field>,
}

#[derive(Debug, Clone)]
pub struct Field {
    name: String,
    argument: Option<String>,
}

fn parse_raw_symbols(lines: Vec<&str>) -> Vec<RawSymbol> {
    let mut symbols = Vec::new();

    for line in lines {
        if line.len() == 0 {
            continue;
        }

        let indent = line.chars().take_while(|x| *x == ' ').count();

        let remaining = line.chars().skip(indent).collect::<String>();

        let objects = remaining.split_terminator(' ').collect::<Vec<_>>();
        let objects = objects
            .into_iter()
            .map(parse_identifier)
            .collect::<Vec<_>>();

        symbols.push(RawSymbol { indent, objects })
    }

    symbols
}

pub fn parse() {
    let symbols = std::fs::read_to_string("locals.symbols").unwrap();

    let ast = symbols.split(" == BOUND SYNTAX TREE == \n").last().unwrap();
    let ast = ast.split("\n").map(|x| x.trim_end()).collect::<Vec<&str>>();

    let symbols = parse_raw_symbols(ast);

    // add one 0-level at the end to pop everything
    let mut symbols = symbols;
    symbols.push(RawSymbol {
        indent: 0,
        objects: vec![],
    });
    let symbols = symbols;

    #[derive(Debug)]
    struct IndexedRawSymbol<'a> {
        idx: usize,
        sym: &'a RawSymbol,
    }

    let mut callstack: VecDeque<IndexedRawSymbol> = VecDeque::new();

    let global_list = &symbols[0];
    assert_eq!(global_list.objects[0].name, "GLOBAL_LIST");
    callstack.push_back(IndexedRawSymbol {
        idx: 0,
        sym: &global_list,
    });

    let mut graph = vec![vec![]; symbols.len()];

    for (idx, sym) in symbols.iter().enumerate().skip(1) {
        while sym.indent < callstack.back().unwrap().sym.indent {
            let mut arguments = vec![];

            // we are at a lower indentation level, pop parents until we are at the same level
            let target_indentation = callstack.back().unwrap().sym.indent - 1;

            while target_indentation < callstack.back().unwrap().sym.indent {
                arguments.push(callstack.pop_back().unwrap());
            }

            let parent = callstack.back().unwrap();

            //println!(
            //    "> Calling {} (idx {}) with {} arguments",
            //    parent.sym.objects[0].name,
            //    parent.idx,
            //    arguments.len()
            //);
            //for a in &arguments {
            //    println!(
            //        "> > {} {:?}",
            //        a.sym.objects[0].name, a.sym.objects[0].argument
            //    );
            //}

            graph[parent.idx].extend(arguments.iter().rev().map(|a| a.idx));
        }

        callstack.push_back(IndexedRawSymbol { idx, sym });

        //println!("{}{:?}", " ".repeat(sym.indent), sym.objects);
    }

    assert_eq!(callstack.len(), 2); // GLOBAL_LIST, and the dummy one
    
    // stuff
    
    generate(&symbols, &graph, 0);
}

fn generate(syms: &Vec<RawSymbol>, graph: &Vec<Vec<usize>>, idx: usize) -> Node {
    // first generate any children
    let mut children = Vec::new();
    for ci in &graph[idx] {
        let ch = generate(syms, graph, *ci);
        children.push(ch);
    }
    
    // then generate this one
    let node = generate_node_good(Entry(syms[idx].objects.clone()), &children);
    
    node
}
