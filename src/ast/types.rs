use super::Field;

#[derive(Debug, Clone)]
pub struct Identifier {
    pub name: String,
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
struct ArrayIndexing {
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

        let blocks = statements.filter_map(|statement| match statement {
            Statement::Block(b) => Some(b),
            _ => None,
        });

        for b in blocks {
            vars.extend(b.recursive_vars());
        }

        vars
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
struct ReturnStatement(Expression);

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

impl TryInto<Vec<Globals>> for Node {
    type Error = NodeExtractError;

    fn try_into(self) -> Result<Vec<Globals>, Self::Error> {
        match self {
            Node::GlobalList(n) => Ok(n),
            _ => Err(NodeExtractError::Unexpected(self)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Declaration {
    pub names: Vec<Identifier>,
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

    // other
    Call(LocatedIdentifier, Vec<LocatedIdentifier>),
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
    ReturnStatement(ReturnStatement),
    StatementList(Vec<Statement>),

    ArgumentList(ArgumentList),

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
    Return(ReturnStatement),
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
            _ => Err(NodeExtractError::Unexpected(value)),
        }
    }
}

#[derive(Debug, Clone)]
struct ArgumentList {
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

            Node::IfStatement(IfStatement {
                condition: relation,
                statement: Box::new(statement),
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
        _ => panic!("Unknown type {}", name),
    };
}
