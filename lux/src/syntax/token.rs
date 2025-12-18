use crate::syntax::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Token { kind, span }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Literals
    Int(i64),
    Float(f64),
    String(String),
    Atom(String),      // :name
    Bool(bool),

    // Identifiers
    Ident(String),     // lowercase start
    TypeIdent(String), // uppercase start

    // Keywords
    Fn,
    Let,
    If,
    Else,
    Match,
    Enum,
    Type,
    Mod,
    Spawn,
    Send,
    Receive,
    After,    // after (for receive timeout)
    SelfKw,   // self
    Extern,
    Return,
    Pub,

    // Operators
    Plus,      // +
    Minus,     // -
    Star,      // *
    Slash,     // /
    Percent,   // %
    Eq,        // =
    EqEq,      // ==
    NotEq,     // !=
    Lt,        // <
    LtEq,      // <=
    Gt,        // >
    GtEq,      // >=
    And,       // &&
    Or,        // ||
    Not,       // !
    Arrow,     // ->
    FatArrow,  // =>
    Pipe,      // |
    Colon2,    // ::

    // Delimiters
    LParen,    // (
    RParen,    // )
    LBrace,    // {
    RBrace,    // }
    LBracket,  // [
    RBracket,  // ]
    Comma,     // ,
    Colon,     // :
    Semi,      // ;
    Dot,       // .

    // Special
    Eof,
    Error(String),
    Newline,
}

impl TokenKind {
    pub fn keyword(s: &str) -> Option<TokenKind> {
        match s {
            "fn" => Some(TokenKind::Fn),
            "let" => Some(TokenKind::Let),
            "if" => Some(TokenKind::If),
            "else" => Some(TokenKind::Else),
            "match" => Some(TokenKind::Match),
            "enum" => Some(TokenKind::Enum),
            "type" => Some(TokenKind::Type),
            "mod" => Some(TokenKind::Mod),
            "spawn" => Some(TokenKind::Spawn),
            "send" => Some(TokenKind::Send),
            "receive" => Some(TokenKind::Receive),
            "after" => Some(TokenKind::After),
            "self" => Some(TokenKind::SelfKw),
            "extern" => Some(TokenKind::Extern),
            "return" => Some(TokenKind::Return),
            "pub" => Some(TokenKind::Pub),
            "true" => Some(TokenKind::Bool(true)),
            "false" => Some(TokenKind::Bool(false)),
            _ => None,
        }
    }
}
