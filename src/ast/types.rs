struct Identifier {
    name: String,
}

struct LocatedIdentifier {
    name: Identifier,
    location: Location,
}

struct Parameter(Identifier);

struct Function {
    name: Identifier,
    parameters: Vec<Parameter>,
    block: Block,

    return_statement: Block,
}

struct Block {
    children: Vec<BlockChild>,
}

enum BlockChild {
    StatementList(Vec<Statement>),
    DeclarationList(Vec<Declaration>),
    ReturnStatement,
}

struct Declaration {
    names: Vec<Identifier>,
}

enum Expression {
    Variable(LocatedIdentifier),

    // operations
    Plus(Box<Expression>, Box<Expression>),
    Minus(Box<Expression>, Box<Expression>),
    Multiply(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),
    Negative(Box<Expression>),
}

enum Statement {
    Assignment(AssignmentStatement),
    Print(PrintStatement),
    If(IfStatement),
    Block(Block),
}

struct PrintStatement {
    args: Vec<(Identifier, Location)>,
}

struct IfStatement {
    condition: Relation,
    statement: Box<Statement>,
}

enum Location {
    Parameter(i32),
    Local(i32),
}

struct Relation {
    // todo
}

struct AssignmentStatement {
    left: LocatedIdentifier,
    right: Expression,
}
