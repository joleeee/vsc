use std::io::Write;

use crate::ast::{BlockChild, Statement};

use super::{Block, Function, Globals, ParsedProgram};
use crate::ast::types::Compilable;

#[derive(Debug)]
struct GlobalSymbol {
    name: String,
    node: Globals,
}

impl ParsedProgram {
    fn globals(&self) -> Vec<GlobalSymbol> {
        let globals: Vec<Globals> = self.clone().root.try_into().unwrap();

        let mut vec = Vec::new();

        println!("instance (globals: {})", globals.len());
        for g in globals {
            println!("One: {:?}", g);
            match g {
                Globals::Function(ref function) => {
                    vec.push(GlobalSymbol {
                        name: function.name.name.clone(),
                        node: g.clone(),
                    });
                }
                Globals::VarDeclaration(ref decl) => {
                    vec.push(GlobalSymbol {
                        name: decl.name.clone(),
                        node: g.clone(), // reference to declaration
                    });
                }
                Globals::ArrayDeclaration(ref array) => {
                    vec.push(GlobalSymbol {
                        name: array.name.name.clone(),
                        node: g.clone(),
                    });
                }
            };
        }

        vec
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

    fn gvars(&self) -> String {
        let mut output = String::new();

        let globals = self.globals();

        let global_arrays = globals.iter().filter_map(|g| match g.node {
            Globals::ArrayDeclaration(ref a) => Some(a),
            _ => None,
        });

        for garr in global_arrays {
            output += &format!("\n\ngarray{}: .zero {}\n", garr.name.name, garr.len * 8);
        }

        for v in &globals {
            println!("Some stuff: {:?}", v);
        }

        let global_vars = globals.iter().filter_map(|g| match g.node {
            Globals::VarDeclaration(ref d) => Some(d),
            _ => None,
        });

        for gvar in global_vars {
            output += &format!("\n\ngvar_{}: .zero 8\n", gvar.name);
        }

        output
    }

    fn functions(&self) -> String {
        const FUN_PROLOGUE: &str = r#"
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

        const FUN_EPILOGUE: &str = r#"
    // epilogue
    movq %rbp, %rsp // reset stack ptr
	popq %rbp
	ret
"#;

        let mut output = String::new();

        let globals = self.globals();
        let functions = globals.iter().filter_map(|g| match g.node {
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
            output += compile_body(f, &f.block, &globals).as_str();

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

"#;

        const CODE: &[u8] = br#"

.text
"#;

        out.write_all(DATA).unwrap();
        out.write_all(self.gvars().as_bytes()).unwrap();

        out.write_all(CODE).unwrap();
        out.write_all(self.functions().as_bytes()).unwrap();

        // main helper
        out.write_all(b"\n\n").unwrap();
        out.write_all(b".globl main\n").unwrap();
        out.write_all(b"main:\n").unwrap();

        // save old base pointer, and set new
        out.write_all(b"    pushq %rbp\n").unwrap();
        out.write_all(b"    movq %rsp, %rbp\n").unwrap();

        // Which registers argc and argv are passed in
        let argc = "rdi";
        let argv = "rsi";

        let globals = self.globals();
        let first = globals
            .iter()
            .filter_map(|g| match g.node {
                Globals::Function(ref f) => Some(f),
                _ => None,
            })
            .next()
            .unwrap();

        let expected_args = first.parameters.len();

        out.write_all(format!("    subq $1, %{argc}\n").as_bytes())
            .unwrap();
        out.write_all(format!("    cmpq ${expected_args}, %{argc}\n").as_bytes())
            .unwrap();
        out.write_all(b"    jne ABORT\n").unwrap();

        if expected_args > 0 {
            // Now we emit a loop to parse all parameters, and push them to the stack,
            // in right-to-left order

            // First move the argv pointer to the vert rightmost parameter
            out.write_all(format!("    addq ${}, %{}\n", expected_args * 8, argv).as_bytes())
                .unwrap();

            // We use rcx as a counter, starting at the number of arguments
            out.write_all(format!("    movq %{}, %rcx\n", argc).as_bytes())
                .unwrap();
            // A loop to parse all parameters
            out.write_all(b"PARSE_ARGV:\n").unwrap();
            // push registers to caller save them
            out.write_all(format!("    pushq %{}\n", argv).as_bytes())
                .unwrap();
            out.write_all(b"    pushq %rcx\n").unwrap();

            // Now call strtol to parse the argument
            // 1st argument, the char *
            out.write_all(format!("    movq (%{}), %rdi\n", argv).as_bytes())
                .unwrap();
            // 2nd argument, a null pointer
            out.write_all(b"    movq $0, %rsi\n").unwrap();
            //3rd argument, we want base 10
            out.write_all(b"    movq $10, %rdx\n").unwrap();
            out.write_all(b"    call strtol\n").unwrap();

            // Restore caller saved registers
            out.write_all(b"    popq %rcx\n").unwrap();
            out.write_all(format!("    popq %{}\n", argv).as_bytes())
                .unwrap();
            // Store the parsed argument on the stack
            out.write_all(b"    pushq %rax\n").unwrap();

            // Point to the previous char*
            out.write_all(format!("    subq $8, %{}\n", argv).as_bytes())
                .unwrap();
            // Loop uses RCX as a counter automatically
            out.write_all(b"    loop PARSE_ARGV\n").unwrap();

            assert!(expected_args <= REGISTERS.len());
            const REGISTERS: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

            // Now, pop up to 6 arguments into registers instead of stack
            for i in 0..expected_args {
                // this will also panic on too many arguments
                out.write_all(format!("    popq %{}\n", REGISTERS[i]).as_bytes())
                    .unwrap();
            }
        }

        out.write_all(format!("    call fun_{}\n", first.name.name).as_bytes())
            .unwrap();
        // Move the return value of the function into RDI
        out.write_all(b"    movq %rax, %rdi\n").unwrap();
        // Exit with the return value as exit code
        out.write_all(b"    call exit\n").unwrap();

        const FOOTER: &[u8] = br#"

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

fn compile_body(function: &Function, block: &Block, _globals: &[GlobalSymbol]) -> String {
    let statement_lists = block.children.iter().filter_map(|c| match c {
        BlockChild::StatementList(ref s) => Some(s),
        _ => None,
    });

    let statements = statement_lists.flatten();

    let mut out = String::new();

    for st in statements {
        out += &match st {
            Statement::Assignment(a) => a.compile(function),
            Statement::Print(p) => p.compile(function),
            //Statement::If(_) => todo!(),
            Statement::Block(b) => compile_body(function, b, _globals),
            Statement::Return(r) => r.compile(function),
            _ => todo!(),
        }
    }

    // then

    out
}
