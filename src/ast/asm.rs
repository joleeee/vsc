use std::{collections::HashMap, io::Write};

use crate::ast::{BlockChild, Statement};

use super::{Function, Globals, Node, ParsedProgram};
use crate::ast::types::Compilable;

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

        for t in &table {
            println!("{}", t.0);
        }

        table
    }

    fn strings(&self) -> String {
        let mut output = String::new();
        for (i, s) in self.string_table.iter().enumerate() {
            output += format!(
                r##"string{:04}:    .asciz "{}"
"##,
                i, s
            )
            .as_str();
        }

        output
    }

    fn vars(&self) -> String {
        const FUN_PROLOGUE: &'static str = r#"
    // prologue
    pushq %rbp // save stack ptr
	movq %rsp, %rbp
	pushq %rdi
	pushq %rsi
	pushq %rdx
	pushq %rcx
	pushq %r8
	pushq %r9
"#;

        const FUN_EPILOGUE: &'static str = r#"
    // epilogue
    movq %rbp, %rsp // reset stack ptr
	popq %rbp
	ret
"#;

        let mut output = String::new();

        let globals = self.globals();
        let functions = globals.values().filter_map(|g| match g.node {
            Globals::Function(ref f) => Some(f),
            _ => None,
        });

        // recursively go through blocks to find all variables
        for f in functions {
            // epilogue
            output += format!("\n\nfun_{}:", f.name.name).as_str();
            output += FUN_PROLOGUE;

            // TODO FIX
            // dummy space since paramters offset the variable indicies
            for _ in 0..f.parameters.len() {
                output += "    pushq $0 // dummy space\n";
            }

            // make space for variables
            let vars_list = f.block.recursive_vars();
            let mut var_count = 0;
            for (block_i, vars) in vars_list.into_iter().enumerate() {
                for var in vars {
                    output += format!(
                        "    pushq $0 // {} ({}) (block {})\n",
                        var.name, var_count, block_i
                    )
                    .as_str();
                    var_count += 1;
                }
            }

            // body
            output += compile_body(&f, &globals).as_str();

            // prologue
            output += "\n    movq $0, %rax // default return value\n";
            output += format!("fun_ret_{}:", f.name.name).as_str();
            output += FUN_EPILOGUE;
        }

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

.text
"#;

        out.write_all(DATA).unwrap();
        out.write_all(self.vars().as_bytes()).unwrap();

        const FOOTER: &[u8] = br#"
.globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $1, %rdi
    cmpq $2, %rdi
    jne ABORT
    addq $16, %rsi
    movq %rdi, %rcx
PARSE_ARGV:
    pushq %rsi
    pushq %rcx
    movq (%rsi), %rdi
    movq $0, %rsi
    movq $10, %rdx
    call strtol
    popq %rcx
    popq %rsi
    pushq %rax
    subq $8, %rsi
    loop PARSE_ARGV
    popq %rdi
    popq %rsi
    call fun_main
    movq %rax, %rdi
    call exit
ABORT:
    leaq errout(%rip), %rdi
    call puts
    movq $1, %rdi
    call exit


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

fn compile_body(function: &Function, globals: &HashMap<String, GlobalSymbol>) -> String {
    let block = &function.block;

    let statement_lists = block.children.iter().filter_map(|c| match c {
        BlockChild::StatementList(ref s) => Some(s),
        _ => None,
    });

    let statements = statement_lists.flatten();

    let mut out = String::new();

    println!("a");
    for st in statements {
        dbg!(&st);
        match st {
            Statement::Assignment(a) => {
                let r = a.compile(function);
                out += &r;
            }
            Statement::Print(p) => {
                let r = p.compile(function);
                out += &r;
            }
            //Statement::If(_) => todo!(),
            //Statement::Block(_) => todo!(),
            Statement::Return(r) => {
                out += &r.compile(function);
            }
            //_ => todo!(),
            _ => (),
        }
    }

    // then

    out
}
