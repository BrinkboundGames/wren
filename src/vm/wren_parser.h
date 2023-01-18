#ifndef wren_parser_h
#define wren_parser_h

#include "wren.h"
#include "wren_vm.h"

typedef struct CompilerBase_t CompilerBase;

// The maximum depth that interpolation can nest. For example, this string has
// three levels:
//
//      "outside %(one + "%(two + "%(three)")")"
#define MAX_INTERPOLATION_NESTING 8

// The different signature syntaxes for different kinds of methods.
typedef enum
{
  // A name followed by a (possibly empty) parenthesized parameter list. Also
  // used for binary operators.
  SIG_METHOD,

  // Just a name. Also used for unary operators.
  SIG_GETTER,

  // A name followed by "=".
  SIG_SETTER,

  // A square bracketed parameter list.
  SIG_SUBSCRIPT,

  // A square bracketed parameter list followed by "=".
  SIG_SUBSCRIPT_SETTER,

  // A constructor initializer function. This has a distinct signature to
  // prevent it from being invoked directly outside of the constructor on the
  // metaclass.
  SIG_INITIALIZER
} SignatureType;

typedef struct
{
  const char* name;
  int length;
  SignatureType type;
  int arity;
} Signature;

typedef enum
{
  TOKEN_LEFT_PAREN,
  TOKEN_RIGHT_PAREN,
  TOKEN_LEFT_BRACKET,
  TOKEN_RIGHT_BRACKET,
  TOKEN_LEFT_BRACE,
  TOKEN_RIGHT_BRACE,
  TOKEN_COLON,
  TOKEN_DOT,
  TOKEN_DOTDOT,
  TOKEN_DOTDOTDOT,
  TOKEN_COMMA,
  TOKEN_STAR,
  TOKEN_SLASH,
  TOKEN_PERCENT,
  TOKEN_HASH,
  TOKEN_PLUS,
  TOKEN_MINUS,
  TOKEN_LTLT,
  TOKEN_GTGT,
  TOKEN_PIPE,
  TOKEN_PIPEPIPE,
  TOKEN_CARET,
  TOKEN_AMP,
  TOKEN_AMPAMP,
  TOKEN_BANG,
  TOKEN_TILDE,
  TOKEN_QUESTION,
  TOKEN_EQ,
  TOKEN_LT,
  TOKEN_GT,
  TOKEN_LTEQ,
  TOKEN_GTEQ,
  TOKEN_EQEQ,
  TOKEN_BANGEQ,

  TOKEN_BREAK,
  TOKEN_CONTINUE,
  TOKEN_CLASS,
  TOKEN_CONSTRUCT,
  TOKEN_ELSE,
  TOKEN_FALSE,
  TOKEN_FOR,
  TOKEN_FOREIGN,
  TOKEN_IF,
  TOKEN_IMPORT,
  TOKEN_AS,
  TOKEN_IN,
  TOKEN_IS,
  TOKEN_NULL,
  TOKEN_RETURN,
  TOKEN_STATIC,
  TOKEN_SUPER,
  TOKEN_THIS,
  TOKEN_TRUE,
  TOKEN_VAR,
  TOKEN_WHILE,

  TOKEN_FIELD,
  TOKEN_STATIC_FIELD,
  TOKEN_NAME,
  TOKEN_NUMBER,

  // A string literal without any interpolation, or the last section of a
  // string following the last interpolated expression.
  TOKEN_STRING,

  // A portion of a string literal preceding an interpolated expression. This
  // string:
  //
  //     "a %(b) c %(d) e"
  //
  // is tokenized to:
  //
  //     TOKEN_INTERPOLATION "a "
  //     TOKEN_NAME          b
  //     TOKEN_INTERPOLATION " c "
  //     TOKEN_NAME          d
  //     TOKEN_STRING        " e"
  TOKEN_INTERPOLATION,

  TOKEN_LINE,

  TOKEN_ERROR,
  TOKEN_EOF
} TokenType;

typedef struct
{
  TokenType type;

  // The beginning of the token, pointing directly into the source.
  const char* start;

  // The length of the token in characters.
  int length;

  // The 1-based line where the token appears.
  int line;

  // The parsed value if the token is a literal.
  Value value;
} Token;

typedef struct
{
  WrenVM* vm;

  // The name of the module being parsed.
  const char* moduleName;

  // The source code being parsed.
  const char* source;

  // The beginning of the currently-being-lexed token in [source].
  const char* tokenStart;

  // The current character being lexed in [source].
  const char* currentChar;

  // The 1-based line number of [currentChar].
  int currentLine;

  // The upcoming token.
  Token next;

  // The most recently lexed token.
  Token current;

  // The most recently consumed/advanced token.
  Token previous;

  // Tracks the lexing state when tokenizing interpolated strings.
  //
  // Interpolated strings make the lexer not strictly regular: we don't know
  // whether a ")" should be treated as a RIGHT_PAREN token or as ending an
  // interpolated expression unless we know whether we are inside a string
  // interpolation and how many unmatched "(" there are. This is particularly
  // complex because interpolation can nest:
  //
  //     " %( " %( inner ) " ) "
  //
  // This tracks that state. The parser maintains a stack of ints, one for each
  // level of current interpolation nesting. Each value is the number of
  // unmatched "(" that are waiting to be closed.
  int parens[MAX_INTERPOLATION_NESTING];
  int numParens;

  // Whether compile errors should be printed to stderr or discarded.
  bool printErrors;

  // If a syntax or compile error has occurred.
  bool hasError;
} Parser;

// Outputs a compile or syntax error. This also marks the compilation as having
// an error, which ensures that the resulting code will be discarded and never
// run. This means that after calling error(), it's fine to generate whatever
// invalid bytecode you want since it won't be used.
//
// You'll note that most places that call error() continue to parse and compile
// after that. That's so that we can try to find as many compilation errors in
// one pass as possible instead of just bailing at the first one.
void error(Parser* parser, const char* format, ...);

void nextToken(Parser* parser);

// Parsing ---------------------------------------------------------------------

// Returns the type of the current token.
TokenType peek(Parser* parser);

// Consumes the current token if its type is [expected]. Returns true if a
// token was consumed.
bool match(Parser* parser, TokenType expected);

// Consumes the current token. Emits an error if its type is not [expected].
void consume(Parser* parser, TokenType expected, const char* errorMessage);

// Matches one or more newlines. Returns true if at least one was found.
bool matchLine(Parser* parser);

// Discards any newlines starting at the current token.
void ignoreNewlines(Parser* parser);

// Consumes the current token. Emits an error if it is not a newline. Then
// discards any duplicate newlines following it.
void consumeLine(Parser* parser, const char* errorMessage);

void allowLineBeforeDot(Parser* parser);

// Grammar ---------------------------------------------------------------------

typedef enum
{
  PREC_NONE,
  PREC_LOWEST,
  PREC_ASSIGNMENT,    // =
  PREC_CONDITIONAL,   // ?:
  PREC_LOGICAL_OR,    // ||
  PREC_LOGICAL_AND,   // &&
  PREC_EQUALITY,      // == !=
  PREC_IS,            // is
  PREC_COMPARISON,    // < > <= >=
  PREC_BITWISE_OR,    // |
  PREC_BITWISE_XOR,   // ^
  PREC_BITWISE_AND,   // &
  PREC_BITWISE_SHIFT, // << >>
  PREC_RANGE,         // .. ...
  PREC_TERM,          // + -
  PREC_FACTOR,        // * / %
  PREC_UNARY,         // unary - ! ~
  PREC_CALL,          // . () []
  PREC_PRIMARY
} Precedence;

typedef void (*GrammarFn)(CompilerBase* compiler, bool canAssign);

typedef void (*SignatureFn)(CompilerBase* compiler, Signature* signature);

typedef struct
{
  GrammarFn prefix;
  GrammarFn infix;
  SignatureFn method;
  Precedence precedence;
  const char* name;
} GrammarRule;

GrammarRule* getRule(TokenType type);
void parsePrecedence(CompilerBase* compiler, Precedence precedence);

bool matchAttribute(CompilerBase* compiler);
void statement(CompilerBase* compiler);
void definition(CompilerBase* compiler);
void expression(CompilerBase* compiler);

Signature signatureFromToken(Parser* parser, SignatureType type);
void validateNumParameters(Parser* parser, int numArgs);
int declareVariable(CompilerBase* compiler, Token* token);
int declareNamedVariable(CompilerBase* compiler);
void finishParameterList(CompilerBase* compiler, Signature* signature);
int addLocal(CompilerBase* compiler, const char* name, int length);
bool finishBlock(CompilerBase* compiler);
void finishArgumentList(CompilerBase* compiler, int* arity);

#endif