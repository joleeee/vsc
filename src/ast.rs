mod asm;
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
        .map(|data| data.chars().take_while(|&x| x != ')').collect::<String>());

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

fn parse_raw_symbol(raw: &str) -> Option<RawSymbol> {
    if raw.is_empty() {
        return None;
    }

    let indent = raw.chars().take_while(|x| *x == ' ').count();
    let remaining = raw.chars().skip(indent).collect::<String>();

    let objects = remaining
        .split_terminator(' ')
        .into_iter()
        .map(parse_identifier)
        .collect::<Vec<_>>();

    Some(RawSymbol { indent, objects })
}

fn parse_symbol_tree(lines: Vec<&str>) -> Vec<RawSymbol> {
    lines
        .into_iter()
        .filter_map(parse_raw_symbol) // removes None
        .collect()
}

fn get_graph(symbols: &[RawSymbol]) -> Vec<Vec<usize>> {
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
        sym: global_list,
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

            graph[parent.idx].extend(arguments.iter().rev().map(|a| a.idx));
        }

        callstack.push_back(IndexedRawSymbol { idx, sym });
    }

    assert_eq!(callstack.len(), 2); // GLOBAL_LIST, and the dummy one

    graph
}

fn parse_string_table(lines: &[&str]) -> Vec<String> {
    let mut out = Vec::new();

    for (i, line) in lines.into_iter().enumerate() {
        let (prefix, data) = line.split_at(2);

        let prefix: usize = prefix.trim().strip_suffix(':').unwrap().parse().unwrap();

        let data = data
            .trim()
            .strip_prefix('"')
            .unwrap()
            .strip_suffix('"')
            .unwrap();

        assert_eq!(i, prefix);

        out.push(data.to_string());
    }

    out
}

#[derive(Debug)]
pub struct ParsedProgram {
    pub string_table: Vec<String>,
    pub root: Node,
}

pub fn parse() -> ParsedProgram {
    let symbols = std::fs::read_to_string("programs/arrays.symbols").unwrap();

    let mut parts = symbols.split("\n == ").skip(1);

    let string_table = parts.next().unwrap();
    let string_table = string_table
        .strip_prefix("STRING LIST == \n")
        .unwrap()
        .split('\n')
        .map(|x| x.trim_end())
        .filter(|v| !v.is_empty())
        .collect::<Vec<&str>>();
    let string_table = parse_string_table(&string_table);

    let ast = parts.next().unwrap();
    let ast = ast.strip_prefix("BOUND SYNTAX TREE == \n").unwrap();
    let ast = ast.split('\n').map(|x| x.trim_end()).collect::<Vec<&str>>();

    let mut symbols = parse_symbol_tree(ast);

    // add one 0-level at the end to pop everything
    symbols.push(RawSymbol {
        indent: 0,
        objects: vec![],
    });
    let symbols = symbols;

    let graph = get_graph(&symbols);

    let root = generate(&symbols, &graph, 0);

    ParsedProgram { string_table, root }
}

fn generate(syms: &Vec<RawSymbol>, graph: &Vec<Vec<usize>>, idx: usize) -> Node {
    // first generate any children
    let children = graph[idx]
        .iter()
        .map(|&i| generate(syms, graph, i))
        .collect();

    // then generate this one
    generate_node_good(Entry(syms[idx].objects.clone()), children)
}
