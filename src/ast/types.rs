use super::Field;

#[derive(Debug, Clone)]
struct Identifier {
    name: String,
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
struct LocatedIdentifier {
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
struct Parameter(Identifier);

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
struct Function {
    name: Identifier,
    parameters: Vec<Parameter>,
    block: Block,

    return_statement: Option<Block>,
}

#[derive(Debug, Clone)]
struct ArrayDeclaration {
    name: Identifier,
    len: i64,
}

#[derive(Debug, Clone)]
struct ArrayIndexing {
    name: LocatedIdentifier,
    idx: Box<Expression>,
}

#[derive(Debug, Clone)]
struct Block {
    children: Vec<BlockChild>,
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
enum BlockChild {
    StatementList(Vec<Statement>),
    DeclarationList(Vec<Declaration>),
    ReturnStatement,
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
struct Declaration {
    names: Vec<Identifier>,
}

#[derive(Debug, Clone)]
enum Expression {
    Variable(LocatedIdentifier),
    Constant(i64),
    Array(ArrayIndexing),

    // operations
    Add(Box<Expression>, Box<Expression>),
    Minus(Box<Expression>, Box<Expression>),
    Multiply(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),
    Negative(Box<Expression>),
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
enum Globals {
    Function(Function),
    Declaration(Declaration),
    ArrayDeclaration(ArrayDeclaration),
}

impl TryFrom<Node> for Globals {
    type Error = NodeExtractError;

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::Function(f) => Ok(Self::Function(f)),
            Node::Declaration(d) => Ok(Self::Declaration(d)),
            Node::ArrayDeclaration(a) => Ok(Self::ArrayDeclaration(a)),
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
    StatementList(Vec<Statement>),

    Identifier(Identifier),
    LocatedIdentifier(LocatedIdentifier),
    NumberData(i64),
    StringData(i64),

    Relation(Relation),
    IfStatement(IfStatement),
}

#[derive(Debug, Clone)]
enum Statement {
    Assignment(AssignmentStatement),
    Print(PrintStatement),
    If(IfStatement),
    Block(Block),
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
            _ => Err(NodeExtractError::Unexpected(value)),
        }
    }
}

#[derive(Debug, Clone)]
struct PrintStatement {
    //args: Vec<(Identifier, Location)>,
    args: Vec<Node>,
}

#[derive(Debug, Clone)]
struct IfStatement {
    condition: Relation,
    statement: Box<Statement>,
}

#[derive(Debug, Clone)]
enum Location {
    Parameter(i64),
    Local(i64),
    GlobalArray(i64),
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
            _x => panic!("Unknown location, got {}", _x),
        }
    }
}

#[derive(Debug, Clone)]
struct Relation {
    left: Expression,
    right: Expression,
    operator: char, // TODO
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
enum Assignee {
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
struct AssignmentStatement {
    left: Assignee,
    right: Expression,
}

pub fn generate_node_good(e: super::Entry, args: &Vec<Node>) -> Node {
    let Field {
        name,
        argument: innerarg,
    } = &e.0[0];

    return match name.as_str() {
        "BLOCK" => Node::Block(Block { children: vec![] }),
        "PARAMETER_LIST" => {
            let data = args
                .clone()
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
                .clone()
                .into_iter()
                .map(TryInto::try_into)
                .map(Result::unwrap)
                .collect();

            Node::Declaration(Declaration { names })
        }
        "DECLARATION_LIST" => {
            let declarations = args
                .clone()
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

            Node::IfStatement(IfStatement {
                condition: relation,
                statement: Box::new(statement),
            })
        }
        "STATEMENT_LIST" => {
            let args = args
                .clone()
                .into_iter()
                .map(TryInto::try_into)
                .map(Result::unwrap)
                .collect();

            Node::StatementList(args)
        }
        "FUNCTION" => {
            let name: Identifier = args[0].clone().try_into().unwrap();
            let parameters: Vec<Parameter> = args[1].clone().try_into().unwrap();
            let block = args[2].clone().try_into().unwrap();

            Node::Function(Function {
                name,
                parameters,
                block,
                return_statement: None,
            })
        }
        "GLOBAL_LIST" => {
            let globals = args
                .clone()
                .into_iter()
                .map(TryInto::try_into)
                .map(Result::unwrap)
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
        _ => panic!("Unknown type {}", name),
    };
}
