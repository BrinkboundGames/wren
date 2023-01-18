#include "wren_common.h"
#include "wren_compiler_ast.h"
#include "wren_ast.h"
#include "wren_ast_json.h"

// INTERESTING NOTES:
// https://lambda.uta.edu/cse5317/notes/node26.html

// Forward declarations
static void classDefinition(CompilerBase* compiler, bool isForeign);
static void variableDefinition(CompilerBase* compiler);
static void attributeDefinition(CompilerBase* compiler, bool runtimeAccess, Value group, Value key, Value value);
static void import(CompilerBase* compiler);
static void breakStatement(CompilerBase* compiler);
static void continueStatement(CompilerBase* compiler);
static void forStatement(CompilerBase* compiler);
static void ifStatement(CompilerBase* compiler);
static void returnStatement(CompilerBase* compiler);
static void whileStatement(CompilerBase* compiler);
static void blockStatement(CompilerBase* compiler);
static void expressionStatement(CompilerBase* compiler);
static void discardAttributes(CompilerBase* compiler);
static int defineModuleVariable(CompilerBase* compiler, Token* token);
static void list(CompilerBase* compiler, bool canAssign);
static void subscript(CompilerBase* compiler, bool canAssign);
static void map(CompilerBase* compiler, bool canAssign);
static void call(CompilerBase* compiler, bool canAssign);
static void infixOp(CompilerBase* compiler, bool canAssign);
static void unaryOp(CompilerBase* compiler, bool canAssign);
static void conditional(CompilerBase* compiler, bool canAssign);
static void boolean(CompilerBase* compiler, bool canAssign);
static void null(CompilerBase* compiler, bool canAssign);
static void super_(CompilerBase* compiler, bool canAssign);
static void this_(CompilerBase* compiler, bool canAssign);
static void field(CompilerBase* compiler, bool canAssign);
static void staticField(CompilerBase* compiler, bool canAssign);
static void name(CompilerBase* compiler, bool canAssign);
static void literal(CompilerBase* compiler, bool canAssign);
static void stringInterpolation(CompilerBase* compiler, bool canAssign);

#pragma optimize( "", off )

// Initializes [compiler].
static void initCompiler(astCompiler* compiler, Parser* parser, astCompiler* parent)
{
  static const struct CompilerOps ops =
  {
    classDefinition,
    variableDefinition,
    attributeDefinition,
    import,
    breakStatement,
    continueStatement,
    forStatement,
    ifStatement,
    returnStatement,
    whileStatement,
    blockStatement,
    expressionStatement,
    discardAttributes,
    defineModuleVariable,
    list,
    subscript,
    map,
    call,
    infixOp,
    unaryOp,
    infixOp, // or
    infixOp, // and
    conditional,
    boolean,
    null,
    super_,
    this_,
    field,
    staticField,
    name,
    literal,
    stringInterpolation
  };

  compiler->base.parser = parser;
  compiler->base.derived = compiler;
  compiler->base.ops = &ops;
  compiler->base.numLocals = 0;
  compiler->base.scopeDepth = 0;

  compiler->parent = parent ? &parent->base : NULL;
  compiler->statements = NULL;
  compiler->statementStack = NULL;
  compiler->expressions = NULL;
  compiler->semanticStack = NULL;
}

Stmt* allocateNewStmt(CompilerBase* compiler)
{
  astCompiler* derived = compiler->derived;
  StmtNode* node = (StmtNode*)malloc(sizeof(StmtNode));
  node->next = derived->statements;
  node->val = (Stmt*)malloc(sizeof(Stmt));
  derived->statements = node;
  return node->val;
}

void pushStmt(CompilerBase* compiler, Stmt* stmt)
{
  astCompiler* derived = compiler->derived;
  StmtNode* node = (StmtNode*)malloc(sizeof(StmtNode));
  node->val = stmt;
  node->next = derived->statementStack;
  derived->statementStack = node;
}

Stmt* popStmt(CompilerBase* compiler)
{
  astCompiler* derived = compiler->derived;
  ASSERT(derived->statementStack != NULL, "Statement Stack underflow!");
  Stmt* ret = derived->statementStack->val;
  StmtNode* temp = derived->statementStack;
  derived->statementStack = derived->statementStack->next;
  free(temp);
  return ret;
}

Stmt* peekStmt(CompilerBase* compiler)
{
  astCompiler* derived = compiler->derived;
  return derived->statementStack ? derived->statementStack->val : NULL;
}

Expr* allocateNewExpr(CompilerBase* compiler)
{
  astCompiler* derived = compiler->derived;
  ExprNode* node = (ExprNode*)malloc(sizeof(ExprNode));
  node->next = derived->expressions;
  node->val = (Expr*)malloc(sizeof(Expr));
  derived->expressions = node;
  return node->val;
}

void pushExpr(CompilerBase* compiler, Expr* expr)
{
  astCompiler* derived = compiler->derived;
  ExprNode* node = (ExprNode*)malloc(sizeof(ExprNode));
  node->val = expr;
  node->next = derived->semanticStack;
  derived->semanticStack = node;
}

Expr* popExpr(CompilerBase* compiler)
{
  astCompiler* derived = compiler->derived;
  ASSERT(derived->semanticStack != NULL, "Semantic Stack underflow!");
  Expr* ret = derived->semanticStack->val;
  ExprNode* temp = derived->semanticStack;
  derived->semanticStack = derived->semanticStack->next;
  free(temp);
  return ret;
}

Expr* peekExpr(CompilerBase* compiler)
{
  astCompiler* derived = compiler->derived;
  return derived->semanticStack ? derived->semanticStack->val : NULL;
}

void wrenGenerateAST(WrenVM* vm, const char* module, const char* source, const char* output)
{
  // Skip the UTF-8 BOM if there is one.
  if (strncmp(source, "\xEF\xBB\xBF", 3) == 0) source += 3;

  Parser parser;
  parser.source = source;
  parser.moduleName = module;
  parser.vm = vm;

  parser.tokenStart = source;
  parser.currentChar = source;
  parser.currentLine = 1;
  parser.numParens = 0;

  // Zero-init the current token. This will get copied to previous when
  // nextToken() is called below.
  parser.next.type = TOKEN_ERROR;
  parser.next.start = source;
  parser.next.length = 0;
  parser.next.line = 0;
  parser.next.value = UNDEFINED_VAL;

  parser.printErrors = true;
  parser.hasError = false;

  // Read the first token into next
  nextToken(&parser);
  // Copy next -> current
  nextToken(&parser);

  astCompiler compiler;
  initCompiler(&compiler, &parser, NULL);
  ignoreNewlines(&parser);

  while (!match(&parser, TOKEN_EOF))
  {
    definition(&compiler.base);

    // If there is no newline, it must be the end of file on the same line.
    if (!matchLine(&parser))
    {
      consume(&parser, TOKEN_EOF, "Expect end of file.");
      break;
    }
  }

  printAstStatementsToJSON(&compiler, output);
}

// Compiles a method definition inside a class body.
//
// Returns `true` if it compiled successfully, or `false` if the method couldn't
// be parsed.
static Method_t* method(CompilerBase* compiler)
{
  // Parse any attributes before the method and store them
  while (matchAttribute(compiler)) {};

  // TODO: What about foreign constructors?
  bool isForeign = match(compiler->parser, TOKEN_FOREIGN);
  bool isStatic = match(compiler->parser, TOKEN_STATIC);

  SignatureFn signatureFn = getRule(compiler->parser->current.type)->method;
  nextToken(compiler->parser);

  if (signatureFn == NULL)
  {
    error(compiler->parser, "Expect method definition.");
    return NULL;
  }

  Method_t* m = (Method_t*)malloc(sizeof(Method_t));
  m->isConstruct = false; // TODO!
  m->isForeign = isForeign;
  m->isStatic = isStatic;
  m->parameters = NULL;
  m->name = compiler->parser->previous;

  int prevNumLocals = compiler->numLocals;

  // Build the method signature.
  Signature signature = signatureFromToken(compiler->parser, SIG_GETTER);

  // Compile the method signature.
  signatureFn(compiler, &signature);

  int currLocalIdx = compiler->numLocals - 1;
  while (currLocalIdx >= prevNumLocals)
  {
      TokenNode* node = (TokenNode*)malloc(sizeof(TokenNode));
      node->val.start = compiler->locals[currLocalIdx].name;
      node->val.length = compiler->locals[currLocalIdx].length;
      node->val.type = TOKEN_NAME;
      node->val.line = 0; // TODO!

      node->next = m->parameters;
      m->parameters = node;

      currLocalIdx--;
  }

  if (!isForeign)
  {
    consume(compiler->parser, TOKEN_LEFT_BRACE, "Expect '{' to begin method body.");
    finishBlock(compiler);
  }

  return m;
}

// Compiles a class definition. Assumes the "class" token has already been
// consumed (along with a possibly preceding "foreign" token).
static void classDefinition(CompilerBase* compiler, bool isForeign)
{
  Stmt* stmt = allocateNewStmt(compiler);
  stmt->tag = ClassStmt;
  stmt->op.classStmt.foreign = isForeign;
  stmt->op.classStmt.methods = NULL;

  // read in the class name
  consume(compiler->parser, TOKEN_NAME, "Expect variable name.");

  stmt->op.classStmt.name = compiler->parser->previous;

  // Load the superclass (if there is one).
  if (match(compiler->parser, TOKEN_IS))
  {
    // TODO: bytecode compiler parses an expression ..
    // do we need to here, as well?
    consume(compiler->parser, TOKEN_NAME, "Expect name for the superclass.");
    stmt->op.classStmt.superclass = compiler->parser->previous;
  }
  else
  {
    stmt->op.classStmt.superclass.type = TOKEN_ERROR;
  }

  // Compile the method definitions.
  consume(compiler->parser, TOKEN_LEFT_BRACE, "Expect '{' after class declaration.");
  matchLine(compiler->parser);

  while (!match(compiler->parser, TOKEN_RIGHT_BRACE))
  {
    Method_t* m = method(compiler);
    if (!m)
    {
        // there was an issue ...
        break;
    }

    // pre-pend the method to the class' linked list
    MethodNode* node = (MethodNode*)malloc(sizeof(MethodNode));
    node->val = m;
    node->next = stmt->op.classStmt.methods;
    stmt->op.classStmt.methods = node;

    // Don't require a newline after the last definition.
    if (match(compiler->parser, TOKEN_RIGHT_BRACE)) break;

    consumeLine(compiler->parser, "Expect newline after definition in class.");
  }

  pushStmt(compiler, stmt);
}

// Compiles a "var" variable definition statement.
static void variableDefinition(CompilerBase* compiler)
{
  // Grab the variable name
  consume(compiler->parser, TOKEN_NAME, "Expect variable name.");

  Stmt* stmt = allocateNewStmt(compiler);
  stmt->tag = VarStmt;
  stmt->op.varStmt.name = compiler->parser->previous;

  // Compile the initializer.
  if (match(compiler->parser, TOKEN_EQ))
  {
    ignoreNewlines(compiler->parser);
    expression(compiler);
  }
  else
  {
    // Default initialize it to null.
    null(compiler, false);
  }

  stmt->op.varStmt.initializer = popExpr(compiler);
  pushStmt(compiler, stmt);
}

static void attributeDefinition(CompilerBase* compiler, bool runtimeAccess, Value group, Value key, Value value)
{
  // TODO
}

static void import(CompilerBase* compiler)
{
  Stmt* stmt = allocateNewStmt(compiler);
  stmt->tag = ImportStmt;

  ignoreNewlines(compiler->parser);
  consume(compiler->parser, TOKEN_STRING, "Expect a string after 'import'.");
  stmt->op.importStmt.path = compiler->parser->previous;

  // The for clause is optional.
  if (!match(compiler->parser, TOKEN_FOR)) return;

  stmt->op.importStmt.variables = NULL;
  AliasNode* last = NULL;

  // Compile the comma-separated list of variables to import.
  do
  {
    ignoreNewlines(compiler->parser);

    consume(compiler->parser, TOKEN_NAME, "Expect variable name.");

    if (!last)
    {
      last = (AliasNode*)malloc(sizeof(AliasNode));
      stmt->op.importStmt.variables = last;
    }
    else
    {
      last->next = (AliasNode*)malloc(sizeof(AliasNode));
      last = last->next;
    }

    last->next = NULL;
    last->val.val = compiler->parser->previous;

    // Store the symbol we care about for the variable
    int slot = -1;
    if(match(compiler->parser, TOKEN_AS))
    {
      consume(compiler->parser, TOKEN_NAME, "Expect variable name.");
      last->val.alias = compiler->parser->previous;
    }
    else
    {
      last->val.alias = last->val.val;
    }
  } while (match(compiler->parser, TOKEN_COMMA));

  pushStmt(compiler, stmt);
}

static void breakStatement(CompilerBase* compiler)
{
  Stmt* stmt = allocateNewStmt(compiler);
  stmt->tag = BreakStmt;
  stmt->op.breakStmt.keyword = compiler->parser->previous;
  pushStmt(compiler, stmt);
}

static void continueStatement(CompilerBase* compiler)
{
  Stmt* stmt = allocateNewStmt(compiler);
  stmt->tag = ContinueStmt;
  stmt->op.continueStmt.keyword = compiler->parser->previous;
  pushStmt(compiler, stmt);
}

static void forStatement(CompilerBase* compiler)
{
  Stmt* stmt = allocateNewStmt(compiler);
  stmt->tag = ForStmt;

  consume(compiler->parser, TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
  consume(compiler->parser, TOKEN_NAME, "Expect for loop variable name.");

  stmt->op.forStmt.variable = compiler->parser->previous;

  consume(compiler->parser, TOKEN_IN, "Expect 'in' after loop variable.");
  ignoreNewlines(compiler->parser);

  // Evaluate the sequence expression
  expression(compiler);

  stmt->op.forStmt.iterator = popExpr(compiler);

  consume(compiler->parser, TOKEN_RIGHT_PAREN, "Expect ')' after loop expression.");

  // Evaluate the body of the loop
  statement(compiler);

  stmt->op.forStmt.body = popStmt(compiler);
  pushStmt(compiler, stmt);
}

static void ifStatement(CompilerBase* compiler)
{
  Stmt* stmt = allocateNewStmt(compiler);
  stmt->tag = IfStmt;

  // Compile the condition.
  consume(compiler->parser, TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
  expression(compiler);
  consume(compiler->parser, TOKEN_RIGHT_PAREN, "Expect ')' after if condition.");

  stmt->op.ifStmt.condition = popExpr(compiler);

  // Compile the then branch.
  statement(compiler);

  stmt->op.ifStmt.thenBranch = popStmt(compiler);

  // Compile the else branch if there is one.
  if (match(compiler->parser, TOKEN_ELSE))
  {
    statement(compiler);
    stmt->op.ifStmt.elseBranch = popStmt(compiler);
  }
  else
  {
      stmt->op.ifStmt.elseBranch = NULL;
  }

  pushStmt(compiler, stmt);
}

static void returnStatement(CompilerBase* compiler)
{
  Stmt* stmt = allocateNewStmt(compiler);
  stmt->tag = ReturnStmt;
  stmt->op.returnStmt.keyword = compiler->parser->previous;

  // Compile the return value.
  if (peek(compiler->parser) == TOKEN_LINE)
  {
    // If there's no expression after return, initializers should 
    // return 'this' and regular methods should return null
    stmt->op.returnStmt.value = NULL;
  }
  else
  {
    expression(compiler);
    stmt->op.returnStmt.value = popExpr(compiler);
  }
  pushStmt(compiler, stmt);
}

static void whileStatement(CompilerBase* compiler)
{
  Stmt* stmt = allocateNewStmt(compiler);
  stmt->tag = WhileStmt;

  // Compile the condition.
  consume(compiler->parser, TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
  expression(compiler);
  consume(compiler->parser, TOKEN_RIGHT_PAREN, "Expect ')' after while condition.");

  stmt->op.whileStmt.condition = popExpr(compiler);

  statement(compiler);
  stmt->op.whileStmt.body = popStmt(compiler);

  pushStmt(compiler, stmt);
}

static void blockStatement(CompilerBase* compiler)
{
  Stmt* stmt = allocateNewStmt(compiler);
  stmt->tag = BlockStmt;
  stmt->op.blockStmt.statements = NULL;

  Stmt* previous = peekStmt(compiler);

  // Block statement.
  if (finishBlock(compiler))
  {
    // it was a single expression .. roll it into a ExpressionStatement
    Stmt* exprStmt = allocateNewStmt(compiler);
    exprStmt->tag = ExprStmt;
    exprStmt->op.exprStmt.expression = popExpr(compiler);

    StmtNode* node = (StmtNode*)malloc(sizeof(StmtNode));
    node->next = stmt->op.blockStmt.statements;
    node->val = exprStmt;
    stmt->op.blockStmt.statements = node;
  }
  else
  {
    // pop and push statements while they don't match previous.
    while (peekStmt(compiler) != previous)
    {
      StmtNode* node = (StmtNode*)malloc(sizeof(StmtNode));
      node->next = stmt->op.blockStmt.statements;
      node->val = popStmt(compiler);
      stmt->op.blockStmt.statements = node;
    }
  }

  pushStmt(compiler, stmt);
}

static void expressionStatement(CompilerBase* compiler)
{
  Stmt* stmt = allocateNewStmt(compiler);
  stmt->tag = ExprStmt;

  expression(compiler);
  stmt->op.exprStmt.expression = popExpr(compiler);

  pushStmt(compiler, stmt);
}

static void discardAttributes(CompilerBase* compiler)
{
  // TODO
}

static int defineModuleVariable(CompilerBase* compiler, Token* token)
{
  // no-op(?)
  return 0;
}

static void list(CompilerBase* compiler, bool canAssign)
{
  Expr* expr = allocateNewExpr(compiler);
  expr->tag = ListExpr;
  expr->op.listExpr.leftBracket = compiler->parser->previous;

  // Compile the list elements.
  int count = 0;
  do
  {
    ignoreNewlines(compiler->parser);

    // Stop if we hit the end of the list.
    if (peek(compiler->parser) == TOKEN_RIGHT_BRACKET) break;

    // The element.
    expression(compiler);
    count++;
  } while (match(compiler->parser, TOKEN_COMMA));

  for (int i = 0; i < count; i++)
  {
    ExprNode* node = (ExprNode*)malloc(sizeof(ExprNode));
    node->val = popExpr(compiler);
    node->next = expr->op.listExpr.elements;
    expr->op.listExpr.elements = node;
  }

  // Allow newlines before the closing ']'.
  ignoreNewlines(compiler->parser);
  consume(compiler->parser, TOKEN_RIGHT_BRACKET, "Expect ']' after list elements.");

  expr->op.listExpr.rightBracket = compiler->parser->previous;

  pushExpr(compiler, expr);
}

// Compiles a read or assignment to [variable].
// Returns true if an expression was pushed onto the stack.
static bool bareName(CompilerBase* compiler, bool canAssign)
{
  bool exprPushed = false;

  // If there's an "=" after a field name, it's an assignment.
  if (canAssign && match(compiler->parser, TOKEN_EQ))
  {
    Expr* assignmentExpr = allocateNewExpr(compiler);
    assignmentExpr->tag = AssignmentExpr;
    assignmentExpr->op.assignmentExpr.target = popExpr(compiler);
    assignmentExpr->op.assignmentExpr.equals = compiler->parser->previous;

    // Compile the right-hand side.
    expression(compiler);

    assignmentExpr->op.assignmentExpr.value = popExpr(compiler);
    pushExpr(compiler, assignmentExpr);
    exprPushed = true;
  }

  allowLineBeforeDot(compiler->parser);
  return exprPushed;
}

// Subscript or "array indexing" operator like `foo[bar]`.
static void subscript(CompilerBase* compiler, bool canAssign)
{
  Expr* expr = allocateNewExpr(compiler);
  expr->tag = SubscriptExpr;
  expr->op.subscriptExpr.leftBracket = compiler->parser->previous;
  expr->op.subscriptExpr.receiver = popExpr(compiler);
  expr->op.subscriptExpr.args = NULL;

  // Parse the argument list.
  int arity = 0;
  finishArgumentList(compiler, &arity);
  consume(compiler->parser, TOKEN_RIGHT_BRACKET, "Expect ']' after arguments.");
  expr->op.subscriptExpr.rightBracket = compiler->parser->previous;
  allowLineBeforeDot(compiler->parser);

  // fill the Expr args
  for (int i = 0; i < arity; i++)
  {
    ExprNode* node = (ExprNode*)malloc(sizeof(ExprNode));
    node->val = popExpr(compiler);
    node->next = expr->op.subscriptExpr.args;
    expr->op.subscriptExpr.args = node;
  }

  pushExpr(compiler, expr);
  bareName(compiler, canAssign);
}

static void map(CompilerBase* compiler, bool canAssign)
{
  Expr* expr = allocateNewExpr(compiler);
  expr->tag = MapExpr;
  expr->op.mapExpr.leftBrace = compiler->parser->previous;
  expr->op.mapExpr.keys = NULL;
  expr->op.mapExpr.values = NULL;

  // Compile the map elements. Each one is compiled to just invoke the
  // subscript setter on the map.
  int count = 0;
  do
  {
    ignoreNewlines(compiler->parser);

    // Stop if we hit the end of the map.
    if (peek(compiler->parser) == TOKEN_RIGHT_BRACE) break;

    // The key.
    parsePrecedence(compiler, PREC_UNARY);
    consume(compiler->parser, TOKEN_COLON, "Expect ':' after map key.");
    ignoreNewlines(compiler->parser);

    // The value.
    expression(compiler);

    count++;
  } while (match(compiler->parser, TOKEN_COMMA));

  // Allow newlines before the closing '}'.
  ignoreNewlines(compiler->parser);
  consume(compiler->parser, TOKEN_RIGHT_BRACE, "Expect '}' after map entries.");

  expr->op.mapExpr.rightBrace = compiler->parser->previous;

  // Fill the key/value pairs, pairwise
  for (int i = 0; i < count; i++)
  {
    // Value
    ExprNode* valueNode = (ExprNode*)malloc(sizeof(ExprNode));
    valueNode->next = expr->op.mapExpr.values;
    valueNode->val = popExpr(compiler);
    expr->op.mapExpr.values = valueNode;

    // Key
    ExprNode* keyNode = (ExprNode*)malloc(sizeof(ExprNode));
    keyNode->next = expr->op.mapExpr.keys;
    keyNode->val = popExpr(compiler);
    expr->op.mapExpr.keys = keyNode;
  }

  pushExpr(compiler, expr);
}

// Compiles an (optional) argument list for a method call with [signature].
static void methodCall(CompilerBase* compiler, Signature* signature)
{
  Expr* expr = popExpr(compiler);
  ASSERT(expr->tag == CallExpr || expr->tag == SuperExpr,
      "Expected a CallExpr or SuperExpr on the semanticStack!");

  // Make a new signature that contains the updated arity and type based on
  // the arguments we find.
  Signature called = { signature->name, signature->length, SIG_GETTER, 0 };

  // Parse the argument list, if any.
  if (match(compiler->parser, TOKEN_LEFT_PAREN))
  {
    called.type = SIG_METHOD;

    // Allow new line before an empty argument list
    ignoreNewlines(compiler->parser);

    // Allow empty an argument list.
    if (peek(compiler->parser) != TOKEN_RIGHT_PAREN)
    {
      finishArgumentList(compiler, &called.arity);
    }
    consume(compiler->parser, TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");

    // pop and push onto Expr "arguments"
    ExprNode** arguments = expr->tag == CallExpr ? &expr->op.callExpr.arguments : &expr->op.superExpr.arguments;
    for (int i = 0; i < called.arity; i++)
    {
      ExprNode* exprNode = (ExprNode*)malloc(sizeof(ExprNode));
      exprNode->next = *arguments;
      exprNode->val = popExpr(compiler);
      *arguments = exprNode;
    }
  }

  // Parse the block argument, if any.
  if (match(compiler->parser, TOKEN_LEFT_BRACE))
  {
    // Include the block argument in the arity.
    called.type = SIG_METHOD;
    called.arity++;

    // Make a dummy signature to track the arity.
    Signature fnSignature = { "", 0, SIG_METHOD, 0 };

    // Parse the parameter list, if any.
    if (match(compiler->parser, TOKEN_PIPE))
    {
      finishParameterList(compiler, &fnSignature);
      consume(compiler->parser, TOKEN_PIPE, "Expect '|' after function parameters.");
    }

    bool isExpressionBody = finishBlock(compiler);
    // TODO: push blockArgument!
  }

  // TODO: Allow Grace-style mixfix methods?

  // If this is a super() call for an initializer, make sure we got an actual
  // argument list.
  if (signature->type == SIG_INITIALIZER)
  {
    if (called.type != SIG_METHOD)
    {
      error(compiler->parser, "A superclass constructor must have an argument list.");
    }

    called.type = SIG_INITIALIZER;
  }

  pushExpr(compiler, expr);
}

// Compiles a call whose name is the previously consumed token. This includes
// getters, method calls with arguments, and setter calls.
static void namedCall(CompilerBase* compiler, bool canAssign)
{
  // Get the token for the method name.
  Signature signature = signatureFromToken(compiler->parser, SIG_GETTER);

  if (canAssign && match(compiler->parser, TOKEN_EQ))
  {
    ignoreNewlines(compiler->parser);

    // Compile the assigned value.
    expression(compiler);

    // TODO: need to push assignmentexpr?
  }
  else
  {
    methodCall(compiler, &signature);
    allowLineBeforeDot(compiler->parser);
  }
}

static void call(CompilerBase* compiler, bool canAssign)
{
  Expr* expr = allocateNewExpr(compiler);
  expr->tag = CallExpr;

  expr->op.callExpr.receiver = popExpr(compiler);

  ignoreNewlines(compiler->parser);
  consume(compiler->parser, TOKEN_NAME, "Expect method name after '.'.");

  expr->op.callExpr.name = compiler->parser->previous;
  expr->op.callExpr.arguments = NULL;
  expr->op.callExpr.blockArgument = NULL;

  // first, push the partially completed callExpr onto the stack
  pushExpr(compiler, expr);

  // resolve arguments/blockArgument
  namedCall(compiler, canAssign);
}

static void infixOp(CompilerBase* compiler, bool canAssign)
{
  GrammarRule* rule = getRule(compiler->parser->previous.type);

  Expr* expr = allocateNewExpr(compiler);
  expr->tag = InfixExpr;
  expr->op.infixExpr.operator = compiler->parser->previous;

  // An infix operator cannot end an expression.
  ignoreNewlines(compiler->parser);

  // Compile the right-hand side.
  parsePrecedence(compiler, (Precedence)(rule->precedence + 1));

  expr->op.infixExpr.right = popExpr(compiler);
  expr->op.infixExpr.left = popExpr(compiler);

  pushExpr(compiler, expr);
}

// Unary operators like `-foo`.
static void unaryOp(CompilerBase* compiler, bool canAssign)
{
  GrammarRule* rule = getRule(compiler->parser->previous.type);

  Expr* expr = allocateNewExpr(compiler);
  expr->tag = PrefixExpr;
  expr->op.prefixExpr.operator = compiler->parser->previous;

  ignoreNewlines(compiler->parser);

  // Compile the argument.
  parsePrecedence(compiler, (Precedence)(PREC_UNARY + 1));

  expr->op.prefixExpr.operand = popExpr(compiler);
  pushExpr(compiler, expr);
}

// conditional: logicalOr ( "?" conditional ":" assignment )?
static void conditional(CompilerBase* compiler, bool canAssign)
{
  Expr* expr = allocateNewExpr(compiler);
  expr->tag = ConditionalExpr;
  expr->op.conditionalExpr.question = compiler->parser->previous;
  expr->op.conditionalExpr.condition = popExpr(compiler); // TODO: is this right?

  // Ignore newline after '?'.
  ignoreNewlines(compiler->parser);

  // Compile the then branch.
  parsePrecedence(compiler, PREC_CONDITIONAL);

  expr->op.conditionalExpr.thenBranch = popExpr(compiler);

  consume(compiler->parser, TOKEN_COLON,
          "Expect ':' after then branch of conditional operator.");

  expr->op.conditionalExpr.colon = compiler->parser->previous;

  ignoreNewlines(compiler->parser);

  parsePrecedence(compiler, PREC_ASSIGNMENT);

  expr->op.conditionalExpr.elseBranch = popExpr(compiler);
  pushExpr(compiler, expr);
}

static void boolean(CompilerBase* compiler, bool canAssign)
{
  Expr* expr = allocateNewExpr(compiler);
  expr->tag = BoolExpr;
  expr->op.tokenExpr = compiler->parser->previous;
  pushExpr(compiler, expr);
}

static void null(CompilerBase* compiler, bool canAssign)
{
  Expr* expr = allocateNewExpr(compiler);
  expr->tag = NullExpr;
  expr->op.tokenExpr = compiler->parser->previous;
  pushExpr(compiler, expr);
}

static void super_(CompilerBase* compiler, bool canAssign)
{
  Expr* expr = allocateNewExpr(compiler);
  expr->tag = SuperExpr;
  expr->op.superExpr.name = compiler->parser->previous;
  expr->op.superExpr.arguments = NULL;
  expr->op.superExpr.blockArgument = NULL;
  pushExpr(compiler, expr);

  // See if it's a named super call, or an unnamed one.
  if (match(compiler->parser, TOKEN_DOT))
  {
    // Compile the superclass call.
    consume(compiler->parser, TOKEN_NAME, "Expect method name after 'super.'.");
    namedCall(compiler, canAssign);
  }
  else
  {
    // No explicit name, so assume the enclosing method.
    // TODO: report error if assumption is NOT correct?

    // Make a dummy signature to track the arity.
    Signature signature = { "", 0, SIG_METHOD, 0 };
    methodCall(compiler, &signature);
  }
}

static void this_(CompilerBase* compiler, bool canAssign)
{
  Expr* expr = allocateNewExpr(compiler);
  expr->tag = ThisExpr;
  expr->op.tokenExpr = compiler->parser->previous;
  pushExpr(compiler, expr);
}

static void field(CompilerBase* compiler, bool canAssign)
{
  Expr* expr = allocateNewExpr(compiler);
  expr->tag = FieldExpr;
  expr->op.tokenExpr = compiler->parser->previous;
  pushExpr(compiler, expr);
  bareName(compiler, canAssign); // check for assignment
}

static void staticField(CompilerBase* compiler, bool canAssign)
{
  Expr* expr = allocateNewExpr(compiler);
  expr->tag = StaticFieldExpr;
  expr->op.tokenExpr = compiler->parser->previous;
  pushExpr(compiler, expr);
  bareName(compiler, canAssign); // check for assignment
}

// Compiles a variable name or method call with an implicit receiver.
static void name(CompilerBase* compiler, bool canAssign)
{
  Expr* expr = allocateNewExpr(compiler);
  expr->tag = CallExpr;
  expr->op.callExpr.name = compiler->parser->previous;
  expr->op.callExpr.receiver = NULL; // implicit receiver
  pushExpr(compiler, expr);

  namedCall(compiler, canAssign); // check for assignment / methodCall
}

static void literal(CompilerBase* compiler, bool canAssign)
{
  Expr* expr = allocateNewExpr(compiler);
  switch (compiler->parser->previous.type)
  {
  case TOKEN_NUMBER:
    expr->tag = NumExpr;
    expr->op.tokenExpr = compiler->parser->previous;
    break;
  case TOKEN_INTERPOLATION:
      __fallthrough;
  case TOKEN_STRING:
      expr->tag = StringExpr;
      expr->op.tokenExpr = compiler->parser->previous;
      break;
  default:
      ASSERT(false, "Unexpected token passed into literal()!");
  }
  pushExpr(compiler, expr);
}

// A string literal that contains interpolated expressions.
//
// Interpolation is syntactic sugar for calling ".join()" on a list. So the
// string:
//
//     "a %(b + c) d"
//
// is compiled roughly like:
//
//     ["a ", b + c, " d"].join()
static void stringInterpolation(CompilerBase* compiler, bool canAssign)
{
  Expr* expr = allocateNewExpr(compiler);
  expr->tag = InterpolationExpr;
  expr->op.interpolationExpr.strings = NULL;
  expr->op.interpolationExpr.expressions = NULL;

  int count = 0;
  do
  {
    // The opening string part.
    literal(compiler, false);

    // The interpolated expression.
    ignoreNewlines(compiler->parser);
    expression(compiler);

    ignoreNewlines(compiler->parser);
    count++;
  } while (match(compiler->parser, TOKEN_INTERPOLATION));

  // The trailing string part.
  consume(compiler->parser, TOKEN_STRING, "Expect end of string interpolation.");
  literal(compiler, false);

  // push trailing string
  ExprNode* node = (ExprNode*)malloc(sizeof(ExprNode));
  node->next = expr->op.interpolationExpr.strings;
  node->val = popExpr(compiler);
  expr->op.interpolationExpr.strings = node;

  // push pairwise
  for (int i = 0; i < count; i++)
  {
    ExprNode* exprNode = (ExprNode*)malloc(sizeof(ExprNode));
    exprNode->next = expr->op.interpolationExpr.expressions;
    exprNode->val = popExpr(compiler);
    expr->op.interpolationExpr.expressions = exprNode;

    ExprNode* strNode = (ExprNode*)malloc(sizeof(ExprNode));
    strNode->next = expr->op.interpolationExpr.strings;
    strNode->val = popExpr(compiler);
    expr->op.interpolationExpr.strings = strNode;
  }

  pushExpr(compiler, expr);
}

#pragma optimize( "", on )
