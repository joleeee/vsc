use std::io::Write;

use crate::{call, emit, jmp, label, leaq, movq, xorq};

use super::{asm::Helper, Field};

static mut LABEL_COUNTER: usize = 0;
struct LabelNumber(usize);

impl LabelNumber {
    fn next() -> Self {
        unsafe {
            LABEL_COUNTER += 1;
            LabelNumber(LABEL_COUNTER)
        }
    }

    pub fn as_if_end(&self) -> String {
        format!("IF_END{}", self.0)
    }

    pub fn as_if_else(&self) -> String {
        format!("IF_ELSE{}", self.0)
    }

    pub fn as_while_start(&self) -> String {
        format!("WHILE_START{}", self.0)
    }

    pub fn as_while_done(&self) -> String {
        format!("WHILE_DONE{}", self.0)
    }
}

pub trait Compilable {
    fn compile<W: Write>(&self, function: &Function, out: &mut W);
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub name: String,
}

impl Identifier {
    pub fn as_global_var(&self) -> String {
        format!("gvar_{}", self.name)
    }

    pub fn as_global_arr(&self) -> String {
        format!("garray_{}", self.name)
    }
}

impl TryFrom<Node> for Identifier {
    type Error = NodeExtractError;

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::Identifier(id) => Ok(id),
            _ => Err(NodeExtractError::Unexpected(value)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LocatedIdentifier {
    name: Identifier,
    location: Location,
}

impl TryFrom<Node> for LocatedIdentifier {
    type Error = NodeExtractError;

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::LocatedIdentifier(id) => Ok(id),
            _ => Err(NodeExtractError::Unexpected(value)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Parameter(Identifier);

impl TryFrom<Node> for Parameter {
    type Error = NodeExtractError;

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::Identifier(i) => Ok(Parameter(i)),
            _ => Err(NodeExtractError::Unexpected(value)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Identifier,
    pub parameters: Vec<Parameter>,
    pub block: Block,

    pub return_statement: Option<Block>,
}

#[derive(Debug, Clone)]
pub struct ArrayDeclaration {
    pub name: Identifier,
    pub len: i64,
}

#[derive(Debug, Clone)]
pub struct ArrayIndexing {
    name: LocatedIdentifier,
    idx: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub children: Vec<BlockChild>,
}
impl Block {
    /// vars which are declared directly inside this block
    pub fn direct_vars(&self) -> Vec<Identifier> {
        let declaration_lists = self.children.iter().filter_map(|c| match c {
            BlockChild::DeclarationList(decls) => Some(decls),
            _ => None,
        });

        let declarations = declaration_lists
            .flat_map(|decls| decls.iter())
            .flat_map(|decl| decl.names.clone());

        declarations.collect()
    }

    // all vars including vars in children
    pub fn recursive_vars(&self) -> Vec<Vec<Identifier>> {
        let mut vars = Vec::new();
        vars.push(self.direct_vars());

        let statement_list = self.children.iter().filter_map(|c| match c {
            BlockChild::StatementList(decls) => Some(decls),
            _ => None,
        });

        let statements = statement_list.flat_map(|decls| decls.iter());

        let blocks = statements.flat_map(|s| s.clone().extract_blocks_recursive());

        for b in blocks {
            vars.extend(b.recursive_vars());
        }

        vars
    }

    pub fn compile<W: Write>(&self, function: &Function, out: &mut W) {
        let statement_lists = self.children.iter().filter_map(|c| match c {
            BlockChild::StatementList(ref s) => Some(s),
            _ => None,
        });

        let statements = statement_lists.flatten();

        for st in statements {
            match st {
                Statement::Assignment(a) => a.compile(function, out),
                Statement::Print(p) => p.compile(function, out),
                Statement::If(i) => i.compile(function, out),
                Statement::Block(b) => b.compile(function, out),
                Statement::Return(r) => r.compile(function, out),
                Statement::While(w) => w.compile(function, out),
                Statement::Break(b) => b.compile(function, out),
            }
        }
    }
}

impl TryFrom<Node> for BlockChild {
    type Error = NodeExtractError;

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::StatementList(s) => Ok(Self::StatementList(s)),
            Node::DeclarationList(s) => Ok(Self::DeclarationList(s)),
            Node::ReturnStatement(s) => Ok(Self::ReturnStatement(s)),
            _ => Err(NodeExtractError::Unexpected(value)),
        }
    }
}

impl TryFrom<Node> for Block {
    type Error = NodeExtractError;

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::Block(b) => Ok(b),
            _ => Err(NodeExtractError::Unexpected(value)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ReturnStatement(Expression);
impl ReturnStatement {
    pub fn compile<W: Write>(&self, function: &Function, out: &mut W) {
        self.0.compile(out);
        jmp!("fun_ret_{}", function.name.name).compile(out);
    }
}

impl TryFrom<Node> for ReturnStatement {
    type Error = NodeExtractError;

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::ReturnStatement(r) => Ok(r),
            _ => Err(NodeExtractError::Unexpected(value)),
        }
    }
}

impl TryInto<Vec<Parameter>> for Node {
    type Error = NodeExtractError;

    fn try_into(self) -> Result<Vec<Parameter>, Self::Error> {
        match self {
            Node::ParameterList(p) => Ok(p),
            _ => Err(NodeExtractError::Unexpected(self)),
        }
    }
}

impl TryInto<i64> for Node {
    type Error = NodeExtractError;

    fn try_into(self) -> Result<i64, Self::Error> {
        match self {
            Node::NumberData(n) => Ok(n),
            _ => Err(NodeExtractError::Unexpected(self)),
        }
    }
}

#[derive(Debug, Clone)]
pub enum BlockChild {
    StatementList(Vec<Statement>),
    DeclarationList(Vec<Declaration>),
    ReturnStatement(ReturnStatement),
}

impl TryFrom<Node> for Declaration {
    type Error = NodeExtractError;

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::Declaration(p) => Ok(p),
            _ => Err(NodeExtractError::Unexpected(value)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Declaration {
    pub names: Vec<Identifier>,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Variable(LocatedIdentifier),
    Constant(i64),
    Array(ArrayIndexing),

    // operations
    Add(Box<Expression>, Box<Expression>),
    Minus(Box<Expression>, Box<Expression>),
    Multiply(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),
    Negative(Box<Expression>),

    // other
    Call(LocatedIdentifier, Vec<LocatedIdentifier>),
}
impl Expression {
    fn compile<W: Write>(&self, out: &mut W) {
        emit!("// expression").compile(out);

        match self {
            Expression::Variable(lid) => {
                let rbp_offset = match lid.location {
                    Location::Parameter(l) => format!("{}(%rbp)", -(l + 1) * 8),
                    Location::GlobalArray(_) | Location::Function(_) => {
                        panic!("Cannot print a whole array or function.")
                    }
                    Location::Local(v) => format!("{}(%rbp)", -(v + 1 + 6) * 8),
                    Location::GlobalVar(_) => format!("{}(%rip)", lid.name.as_global_var()),
                };

                movq!("{}, %rax", rbp_offset).compile(out);
            }
            Expression::Constant(n) => {
                movq!("${}, %rax", n).compile(out);
            }
            Expression::Add(a, b) => {
                a.compile(out);
                emit!("pushq %rax").compile(out);
                b.compile(out);
                emit!("popq %rdi").compile(out);

                emit!("addq %rdi, %rax").compile(out);
            }
            Expression::Minus(a, b) => {
                a.compile(out);
                emit!("pushq %rax").compile(out);
                b.compile(out);
                emit!("popq %rdi").compile(out);

                emit!("subq %rax, %rdi").compile(out);
                emit!("movq %rdi, %rax").compile(out);
            }
            Expression::Multiply(a, b) => {
                a.compile(out);
                emit!("pushq %rax").compile(out);
                b.compile(out);
                emit!("popq %rdi").compile(out);

                emit!("imulq %rdi, %rax").compile(out);
            }
            Expression::Divide(a, b) => {
                a.compile(out);
                emit!("pushq %rax").compile(out);
                b.compile(out);
                emit!("popq %rdi").compile(out);

                // numerator has to be in %rbx + %rax
                // denominator wherever

                // stupid but works, just swap them around first
                // should really have just swapped them around in the first
                // place, but this works
                emit!("movq %rdi, %r11").compile(out);
                emit!("movq %rax, %rdi").compile(out);
                emit!("movq %r11, %rax").compile(out);

                // rdx must be zero, holds upper bits
                emit!("movq %rdx, %rbx").compile(out);
                emit!("cqto").compile(out);
                emit!("idivq %rdi").compile(out);
            }
            Expression::Negative(a) => {
                a.compile(out);
                emit!("negq %rax").compile(out);
            }
            Expression::Call(func, arguments) => {
                // evaluate all arguments
                for (i, arg) in arguments.iter().enumerate() {
                    Expression::Variable(arg.clone()).compile(out);
                    emit!("pushq %rax // save evald {}th arg\n", i).compile(out);
                }

                // put into right registers
                for (i, _) in arguments.iter().enumerate().rev() {
                    const REGISTERS: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

                    emit!("popq %{}\n", REGISTERS[i]).compile(out);
                }

                // actual call
                call!("fun_{}", func.name.name).compile(out);
            }
            Expression::Array(array_indexing) => {
                // 1. evaluate index
                array_indexing.idx.compile(out);

                // 2. load base
                leaq!("{}(%rip), %rdi", array_indexing.name.name.as_global_arr()).compile(out);

                // 3. get element
                movq!("(%rdi, %rax, 8), %rax").compile(out);
            }
        }
    }
}

impl TryFrom<Node> for Expression {
    type Error = NodeExtractError;

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::Expression(p) => Ok(p),
            Node::LocatedIdentifier(i) => Ok(Expression::Variable(i)),
            Node::ArrayIndexing(i) => Ok(Expression::Array(i)),
            Node::NumberData(n) => Ok(Expression::Constant(n)),
            _ => Err(NodeExtractError::Unexpected(value)),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Globals {
    Function(Function),
    VarDeclaration(Identifier),
    ArrayDeclaration(ArrayDeclaration),
}

impl TryFrom<Node> for Vec<Globals> {
    type Error = NodeExtractError;

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::Function(f) => Ok(vec![Globals::Function(f)]),
            Node::Declaration(d) => Ok(d.names.into_iter().map(Globals::VarDeclaration).collect()),
            Node::ArrayDeclaration(a) => Ok(vec![Globals::ArrayDeclaration(a)]),
            Node::GlobalList(n) => Ok(n),
            _ => Err(NodeExtractError::Unexpected(value)),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Node {
    GlobalList(Vec<Globals>),

    Function(Function),
    ArrayDeclaration(ArrayDeclaration),
    ArrayIndexing(ArrayIndexing),

    Block(Block),
    ParameterList(Vec<Parameter>),
    Expression(Expression),

    Declaration(Declaration),
    DeclarationList(Vec<Declaration>),
    AssignmentStatement(AssignmentStatement),
    PrintStatement(PrintStatement),
    ReturnStatement(ReturnStatement),
    StatementList(Vec<Statement>),

    ArgumentList(ArgumentList),

    Identifier(Identifier),
    LocatedIdentifier(LocatedIdentifier),
    NumberData(i64),
    StringData(i64),

    Relation(Relation),
    IfStatement(IfStatement),
    WhileStatement(WhileStatement),
    BreakStatement(BreakStatement),
}

#[derive(Debug, Clone)]
pub enum Statement {
    Assignment(AssignmentStatement),
    Print(PrintStatement),
    If(IfStatement),
    Block(Block),
    Return(ReturnStatement),
    While(WhileStatement),
    Break(BreakStatement),
}

impl Statement {
    pub fn extract_blocks_recursive(self) -> Vec<Block> {
        match self {
            Statement::If(i) => {
                let mut output = vec![];

                for b in i.statement.extract_blocks_recursive() {
                    output.push(b);
                }

                if let Some(statement) = i.else_statement {
                    for b in statement.extract_blocks_recursive() {
                        output.push(b);
                    }
                }

                output
            }
            Statement::Block(b) => vec![b],
            Statement::While(w) => vec![w.statement],
            Statement::Return(_) => vec![], // return only has an expression
            Statement::Assignment(_) => vec![], // assignment only has an expression
            Statement::Print(_) => vec![], // print can technically have anything (Node) but it would crash
            Statement::Break(_) => vec![], // return does not contain any statements
        }
    }
}

impl Compilable for Statement {
    fn compile<W: Write>(&self, function: &Function, out: &mut W) {
        match self {
            Statement::Assignment(a) => a.compile(function, out),
            Statement::Print(p) => p.compile(function, out),
            Statement::If(i) => i.compile(function, out),
            Statement::Block(b) => b.compile(function, out),
            Statement::Return(r) => r.compile(function, out),
            Statement::While(w) => w.compile(function, out),
            Statement::Break(b) => b.compile(function, out),
        }
    }
}

#[derive(Debug)]
pub enum NodeExtractError {
    Unexpected(Node),
}

impl TryFrom<Node> for Statement {
    type Error = NodeExtractError;

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::AssignmentStatement(s) => Ok(Self::Assignment(s)),
            Node::PrintStatement(s) => Ok(Self::Print(s)),
            Node::IfStatement(s) => Ok(Self::If(s)),
            Node::Block(s) => Ok(Self::Block(s)),
            Node::ReturnStatement(s) => Ok(Self::Return(s)),
            Node::WhileStatement(w) => Ok(Self::While(w)),
            Node::BreakStatement(b) => Ok(Self::Break(b)),
            _ => Err(NodeExtractError::Unexpected(value)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ArgumentList {
    arguments: Vec<LocatedIdentifier>,
}

impl TryFrom<Node> for ArgumentList {
    type Error = NodeExtractError;

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::ArgumentList(a) => Ok(a),
            _ => Err(NodeExtractError::Unexpected(value)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct PrintStatement {
    pub args: Vec<Node>,
}

impl Compilable for PrintStatement {
    fn compile<W: Write>(&self, _function: &Function, out: &mut W) {
        emit!("// print").compile(out);

        for arg in &self.args {
            match arg {
                Node::Expression(e) => {
                    e.compile(out);

                    movq!("%rax, %rsi").compile(out);
                    xorq!("%rax, %rax").compile(out);
                    leaq!("intout(%rip), %rdi").compile(out);
                }
                Node::LocatedIdentifier(lid) => {
                    Expression::Variable(lid.clone()).compile(out);

                    movq!("%rax, %rsi").compile(out);
                    xorq!("%rax, %rax").compile(out);
                    leaq!("intout(%rip), %rdi").compile(out);
                }
                //Node::NumberData(_) => todo!(),
                Node::StringData(sidx) => {
                    leaq!("strout(%rip), %rdi").compile(out);
                    leaq!("string{sidx:04}(%rip), %rsi").compile(out);
                }
                Node::ArrayIndexing(ai) => {
                    Expression::Array(ai.clone()).compile(out);

                    movq!("%rax, %rsi").compile(out);
                    xorq!("%rax, %rax").compile(out);
                    leaq!("intout(%rip), %rdi").compile(out);
                }
                _ => todo!(),
            };

            call!("printf").compile(out);
        }

        // terminating endline
        movq!("$'\\n', %rdi").compile(out);
        call!("putchar").compile(out);
    }
}

#[derive(Debug, Clone)]
pub struct IfStatement {
    condition: Relation,
    statement: Box<Statement>,
    else_statement: Option<Box<Statement>>,
}
impl IfStatement {
    fn compile<W: Write>(&self, function: &Function, out: &mut W) {
        emit!("").compile(out);
        emit!("// if statement").compile(out);

        let label = LabelNumber::next();

        self.condition.compile(function, out);

        let inverse_instruction = match self.condition.operator {
            '>' => "jle",
            '<' => "jge",
            '=' => "jne",
            '!' => "je",
            _ => todo!("unknown operator {}", self.condition.operator),
        };

        // jump to else, if condition failed (or end if there is no else)
        let fail_label = match &self.else_statement {
            Some(_) => label.as_if_else(),
            None => label.as_if_end(),
        };
        emit!("{inverse_instruction} {fail_label}").compile(out);

        // otherwise run body
        self.statement.compile(function, out);
        jmp!("{}", label.as_if_end()).compile(out);

        // compile else if it exists
        if let Some(else_statement) = &self.else_statement {
            label!("{}", label.as_if_else());
            else_statement.compile(function, out);
        }

        // end label
        label!("{}", LabelNumber::next().as_if_end());
    }
}

#[derive(Debug, Clone)]
pub struct WhileStatement {
    condition: Relation,
    statement: Block,
}
impl WhileStatement {
    pub fn compile<W: Write>(&self, function: &Function, out: &mut W) {
        emit!("").compile(out);
        emit!("// while statement").compile(out);

        let label = LabelNumber::next();

        let inverse_instruction = match self.condition.operator {
            '>' => "jle",
            '<' => "jge",
            '=' => "jne",
            '!' => "je",
            _ => todo!("unknown operator {}", self.condition.operator),
        };

        label!("{}", label.as_while_start()).compile(out);

        // should end up with a cmp
        self.condition.compile(function, out);

        // jump to end if condition failed
        emit!(
            "{inverse_instruction} {while_done}",
            while_done = label.as_while_done()
        )
        .compile(out);

        // otherwise, run the body
        self.statement.compile(function, out);

        // then jump up again
        jmp!("{}", label.as_while_start()).compile(out);

        // and when done, continue:
        label!("{}", label.as_while_done()).compile(out);
    }
}

#[derive(Debug, Clone)]
pub struct BreakStatement {
    somevar: (),
}

impl BreakStatement {
    pub fn compile<W: Write>(&self, function: &Function, out: &mut W) {
        //emit!("// BREAK (todo)").compile(out);
        todo!();
    }
}

#[derive(Debug, Clone)]
enum Location {
    Parameter(i64),
    Local(i64),
    GlobalArray(i64),
    GlobalVar(i64),
    Function(i64),
}

impl TryFrom<&Field> for Location {
    type Error = NodeExtractError;

    fn try_from(value: &Field) -> Result<Self, Self::Error> {
        let position: i64 = value.argument.as_ref().unwrap().parse().unwrap();

        match value.name.as_str() {
            "LOCAL_VAR" => Ok(Location::Local(position)),
            "PARAMETER" => Ok(Location::Parameter(position)),
            "GLOBAL_ARRAY" => Ok(Location::GlobalArray(position)),
            "FUNCTION" => Ok(Location::Function(position)),
            "GLOBAL_VAR" => Ok(Location::GlobalVar(position)),
            _x => panic!("Unknown location, got {}", _x),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Relation {
    left: Expression,
    right: Expression,
    operator: char, // TODO
}
impl Relation {
    // cmpq the two expressions
    fn compile<W: Write>(&self, _function: &Function, out: &mut W) {
        self.left.compile(out);
        out.write_all(b"    pushq %rax\n").unwrap();

        self.right.compile(out);
        out.write_all(b"    popq %rdi\n").unwrap();

        // rdi: left
        // rax: right

        out.write_all(b"    cmpq %rax, %rdi\n").unwrap();
    }
}

impl TryFrom<Node> for Relation {
    type Error = NodeExtractError;

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::Relation(rel) => Ok(rel),
            _ => Err(NodeExtractError::Unexpected(value)),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Assignee {
    Variable(LocatedIdentifier),
    Array(ArrayIndexing),
}

impl TryFrom<Node> for Assignee {
    type Error = NodeExtractError;

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::ArrayIndexing(a) => Ok(Self::Array(a)),
            Node::LocatedIdentifier(i) => Ok(Self::Variable(i)),
            _ => Err(NodeExtractError::Unexpected(value)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct AssignmentStatement {
    left: Assignee,
    right: Expression,
}

impl Compilable for AssignmentStatement {
    fn compile<W: Write>(&self, _function: &Function, out: &mut W) {
        // 1. evaluate the expression
        self.right.compile(out);

        // 2. move to the right place
        match &self.left {
            Assignee::Variable(var) => {
                let rbp_offset = match var.location {
                    Location::Parameter(l) => format!("{}(%rbp)", -(l + 1) * 8),
                    Location::GlobalArray(_) | Location::Function(_) => {
                        panic!("Cannot write to a whole array or function.")
                    }
                    Location::Local(v) => format!("{}(%rbp)", -(v + 1 + 6) * 8),
                    Location::GlobalVar(_) => format!("{}(%rip)", var.name.as_global_var()),
                };

                movq!("%rax, {} // -> {}", rbp_offset, var.name.name).compile(out);
            }
            Assignee::Array(array_indexing) => {
                movq!("%rax, %r9").compile(out); // r9 := expression

                // 1. evaluate index
                array_indexing.idx.compile(out);

                // 2. load base
                leaq!("{}(%rip), %rdi", array_indexing.name.name.as_global_arr()).compile(out);

                // 3. write to element
                movq!("%r9, (%rdi, %rax, 8)").compile(out);
            }
        };
    }
}

pub fn generate_node_good(e: super::Entry, args: Vec<Node>) -> Node {
    let Field {
        name,
        argument: innerarg,
    } = &e.0[0];

    return match name.as_str() {
        "BLOCK" => Node::Block(Block {
            children: args
                .into_iter()
                .map(TryInto::try_into)
                .map(Result::unwrap)
                .collect(),
        }),

        "PARAMETER_LIST" => {
            let data = args
                .into_iter()
                .map(TryInto::try_into)
                .map(Result::unwrap)
                .collect();

            Node::ParameterList(data)
        }
        "IDENTIFIER_DATA" => {
            let name = innerarg.as_ref().unwrap().clone();
            let id = Identifier { name };

            if let Some(loc) = e.0.get(1) {
                let location = loc.try_into().unwrap();
                Node::LocatedIdentifier(LocatedIdentifier { name: id, location })
            } else {
                Node::Identifier(id)
            }
        }
        "DECLARATION" => {
            let names = args
                .into_iter()
                .map(TryInto::try_into)
                .map(Result::unwrap)
                .collect();

            Node::Declaration(Declaration { names })
        }
        "DECLARATION_LIST" => {
            let declarations = args
                .into_iter()
                .map(TryInto::try_into)
                .map(Result::unwrap)
                .collect();

            Node::DeclarationList(declarations)
        }
        "EXPRESSION" => {
            // lazy eval, in case there is only one arg
            let first = || args[0].clone().try_into().unwrap();
            let second = || args[1].clone().try_into().unwrap();

            let call_args = || {
                let arg_list: ArgumentList = args[1].clone().try_into().unwrap();
                arg_list.arguments
            };

            Node::Expression(match innerarg.as_ref().unwrap().as_str() {
                "+" => Expression::Add(Box::new(first()), Box::new(second())),
                "-" => {
                    if args.len() == 1 {
                        Expression::Negative(Box::new(first()))
                    } else {
                        Expression::Minus(Box::new(first()), Box::new(second()))
                    }
                }
                "*" => Expression::Multiply(Box::new(first()), Box::new(second())),
                "/" => Expression::Divide(Box::new(first()), Box::new(second())),
                "call" => Expression::Call(args[0].clone().try_into().unwrap(), call_args()),
                _ => todo!(),
            })
        }
        "ASSIGNMENT_STATEMENT" => {
            let left = args[0].clone().try_into().unwrap();
            let right = args[1].clone().try_into().unwrap();

            Node::AssignmentStatement(AssignmentStatement { left, right })
        }
        "PRINT_STATEMENT" => Node::PrintStatement(PrintStatement {
            args: args.to_vec(),
        }),
        "NUMBER_DATA" => {
            let n = innerarg.as_ref().unwrap().parse().unwrap();
            Node::NumberData(n)
        }
        "RELATION" => {
            let operator = innerarg.as_ref().unwrap().chars().next().unwrap();

            let left: Expression = args[0].clone().try_into().unwrap();
            let right: Expression = args[1].clone().try_into().unwrap();

            Node::Relation(Relation {
                left,
                right,
                operator,
            })
        }
        "IF_STATEMENT" => {
            let relation: Relation = args[0].clone().try_into().unwrap();
            let statement: Statement = args[1].clone().try_into().unwrap();
            let else_statement: Option<Statement> =
                args.get(2).map(|v| v.clone().try_into().unwrap());

            Node::IfStatement(IfStatement {
                condition: relation,
                statement: Box::new(statement),
                else_statement: else_statement.map(Box::new),
            })
        }
        "STATEMENT_LIST" => {
            let args = args
                .into_iter()
                .map(TryInto::try_into)
                .map(Result::unwrap)
                .collect();

            Node::StatementList(args)
        }
        "FUNCTION" => {
            let name: Identifier = args[0].clone().try_into().unwrap();
            let parameters: Vec<Parameter> = args[1].clone().try_into().unwrap();

            let mut block: Option<Block> = None;
            let mut return_statement: Option<Block> = None;

            for v in &args[2..] {
                let v = v.clone();

                if let Ok(b) = v.clone().try_into() {
                    block = Some(b);
                } else if let Ok(s) = v.clone().try_into() {
                    return_statement = Some(s);
                }
            }

            let block = block.unwrap_or(Block { children: vec![] });

            Node::Function(Function {
                name,
                parameters,
                block,
                return_statement,
            })
        }
        "GLOBAL_LIST" => {
            let globals = args
                .into_iter()
                .map(TryInto::<Vec<Globals>>::try_into)
                .flat_map(Result::unwrap)
                .collect();

            Node::GlobalList(globals)
        }
        "ARRAY_DECLARATION" => {
            let name = args[0].clone().try_into().unwrap();
            let len: i64 = args[1].clone().try_into().unwrap();

            Node::ArrayDeclaration(ArrayDeclaration { name, len })
        }
        "ARRAY_INDEXING" => {
            let name = args[0].clone().try_into().unwrap();
            let idx = Box::new(args[1].clone().try_into().unwrap());

            Node::ArrayIndexing(ArrayIndexing { name, idx })
        }
        "STRING_DATA" => {
            let reference = innerarg.as_ref().unwrap()[1..].parse().unwrap();

            Node::StringData(reference)
        }
        "ARGUMENT_LIST" => {
            let parameters: Vec<LocatedIdentifier> = args
                .into_iter()
                .map(TryInto::try_into)
                .map(Result::unwrap)
                .collect();

            Node::ArgumentList(ArgumentList {
                arguments: parameters,
            })
        }
        "RETURN_STATEMENT" => {
            let exp: Expression = args[0].clone().try_into().unwrap();

            Node::ReturnStatement(ReturnStatement(exp))
        }
        "WHILE_STATEMENT" => {
            let condition: Relation = args[0].clone().try_into().unwrap();
            let statement: Block = args[1].clone().try_into().unwrap();

            Node::WhileStatement(WhileStatement {
                condition,
                statement,
            })
        }
        "BREAK_STATEMENT" => Node::BreakStatement(BreakStatement { somevar: () }),
        _ => panic!("Unknown type {}", name),
    };
}
