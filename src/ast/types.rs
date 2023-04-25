use super::Field;

#[derive(Debug, Clone)]
struct Identifier {
    name: String,
}

#[derive(Debug, Clone)]
struct LocatedIdentifier {
    name: Identifier,
    location: Location,
}

#[derive(Debug, Clone)]
struct Parameter(Identifier);

#[derive(Debug, Clone)]
struct Function {
    name: Identifier,
    parameters: Vec<Parameter>,
    block: Block,

    return_statement: Block,
}

#[derive(Debug, Clone)]
struct Block {
    children: Vec<BlockChild>,
}

#[derive(Debug, Clone)]
enum BlockChild {
    StatementList(Vec<Statement>),
    DeclarationList(Vec<Declaration>),
    ReturnStatement,
}

#[derive(Debug, Clone)]
struct Declaration {
    names: Vec<Identifier>,
}

#[derive(Debug, Clone)]
enum Expression {
    Variable(LocatedIdentifier),
    Constant(i32),

    // operations
    Add(Box<Expression>, Box<Expression>),
    Minus(Box<Expression>, Box<Expression>),
    Multiply(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),
    Negative(Box<Expression>),
}

#[derive(Debug, Clone)]
pub enum Node {
    Function(Function),
    Block(Block),
    ParameterList(Vec<Parameter>),
    Expression(Expression),

    Declaration(Declaration),
    DeclarationList(Vec<Declaration>),
    AssignmentStatement(AssignmentStatement),
    PrintStatement(PrintStatement),

    Identifier(Identifier),
    LocatedIdentifier(LocatedIdentifier),
    NumberData(i32),

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
enum NodeExtractError {
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
    Parameter(i32),
    Local(i32),
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
struct AssignmentStatement {
    left: LocatedIdentifier,
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
            let mut data = vec![];

            for d in args {
                match d {
                    Node::Identifier(p) => data.push(Parameter(p.clone())),
                    _x => panic!("Expected parameter, got {:?}", _x),
                }
            }

            Node::ParameterList(data)
        }
        "IDENTIFIER_DATA" => {
            let name = innerarg.as_ref().unwrap().clone();
            let id = Identifier { name };

            if let Some(loc) = e.0.get(1) {
                let position = loc.argument.as_ref().unwrap().parse().unwrap();
                let location = match loc.name.as_str() {
                    "LOCAL_VAR" => Location::Local(position),
                    "PARAMETER" => Location::Parameter(position),
                    _x => panic!("Unknown location, got {}", _x),
                };

                Node::LocatedIdentifier(LocatedIdentifier { name: id, location })
            } else {
                Node::Identifier(id)
            }
        }
        "DECLARATION" => {
            let mut names = vec![];

            for d in args {
                match d {
                    Node::Identifier(p) => names.push(p.clone()),
                    _x => panic!("Expected parameter, got {:?}", _x),
                }
            }

            Node::Declaration(Declaration { names })
        }
        "DECLARATION_LIST" => {
            let mut declarations = vec![];

            for d in args {
                match d {
                    Node::Declaration(p) => declarations.push((*p).clone()),
                    _x => panic!("Expected parameter, got {:?}", _x),
                }
            }

            Node::DeclarationList(declarations)
        }
        "EXPRESSION" => {
            fn evaluate(x: &Node) -> Expression {
                match x {
                    Node::Expression(p) => p.clone(),
                    Node::LocatedIdentifier(i) => Expression::Variable(i.clone()),
                    _x => panic!("Expected expression, got {:?}", _x),
                }
            }

            // lazy eval, in case there is only one arg
            let first = || evaluate(&args[0]);
            let second = || evaluate(&args[1]);

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
            let left = match &args[0] {
                Node::LocatedIdentifier(id) => id.clone(),
                _x => panic!("Expected identifier, got {:?}", _x),
            };

            let right = match &args[1] {
                Node::Expression(p) => p.clone(),
                Node::LocatedIdentifier(i) => Expression::Variable(i.clone()),
                Node::NumberData(n) => Expression::Constant(*n),
                _x => panic!("Expected expression, got {:?}", _x),
            };

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

            let left = match &args[0] {
                Node::Expression(p) => p.clone(),
                Node::LocatedIdentifier(i) => Expression::Variable(i.clone()),
                Node::NumberData(n) => Expression::Constant(*n),
                _x => panic!("Expected expression, got {:?}", _x),
            };

            let right = match &args[1] {
                Node::Expression(p) => p.clone(),
                Node::LocatedIdentifier(i) => Expression::Variable(i.clone()),
                Node::NumberData(n) => Expression::Constant(*n),
                _x => panic!("Expected expression, got {:?}", _x),
            };

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
        _ => panic!("Unknown type {}", name),
    };
}
