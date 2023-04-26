use std::{collections::HashMap, io::Write};

use super::{Globals, Identifier, Node, ParsedProgram};

#[derive(Debug)]
struct GlobalSymbol {
    name: String,
    node: Globals,
}

impl ParsedProgram {
    fn globals(&self) -> HashMap<String, GlobalSymbol> {
        let globals: Vec<Globals> = self.clone().root.try_into().unwrap();

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

    fn strings(&self) -> String {
        let mut output = String::new();
        for (i, s) in self.string_table.iter().enumerate() {
            output += format!(r##"string{:04}:    .aciz "{}""##, i, s).as_str();
        }

        output
    }

    fn vars(&self) -> String {
        let mut output = String::new();

        output
    }

    pub fn compile<W: Write>(&self, mut out: W) {
        const TEXT: &[u8] = br#".section __TEXT,__text
intout: .asciz "%lld "
strout: .asciz "%s "
errout: .asciz "Wrong number of arguments"

"#;
        out.write_all(TEXT).unwrap();
        out.write_all(self.strings().as_bytes()).unwrap();

        const DATA: &[u8] = br#"

.data
.align 8
"#;

        out.write_all(DATA).unwrap();
        out.write_all(self.vars().as_bytes()).unwrap();

        const FOOTER: &[u8] = br#"

# macOS stuff
.set exit, _exit
.set strtol, _strtol
.set printf, _printf
.set puts, _puts
.set putchar, _putchar
.set _main, main
.globl _main
"#;
        out.write_all(FOOTER).unwrap();
    }
}
