use std::collections::HashMap;

use super::{Globals, Identifier, Node};

#[derive(Debug)]
struct GlobalSymbol {
    name: String,
    node: Globals,
}

impl Node {
    fn globals(&self) -> HashMap<String, GlobalSymbol> {
        let globals: Vec<Globals> = self.clone().try_into().unwrap();

        let mut table = HashMap::new();

        for g in globals {
            match g {
                Globals::Function(ref function) => {
                    table.insert(
                        function.name.name.clone(),
                        GlobalSymbol {
                            name: function.name.name.clone(),
                            node: g.clone(),
                        },
                    );
                }
                Globals::Declaration(ref decl_list) => {
                    for decl in &decl_list.names {
                        table.insert(
                            decl.name.clone(),
                            GlobalSymbol {
                                name: decl.name.clone(),
                                node: g.clone(), // reference to declaration
                            },
                        );
                    }
                }
                Globals::ArrayDeclaration(ref array) => {
                    table.insert(
                        array.name.name.clone(),
                        GlobalSymbol {
                            name: array.name.name.clone(),
                            node: g.clone(),
                        },
                    );
                }
            };
        }

        for t in table {
            println!("{}", t.0);
        }
        todo!();
    }

    pub fn compile(&self) {
        self.globals();
    }
}
