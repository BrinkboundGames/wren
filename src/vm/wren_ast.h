#ifndef wren_ast_h
#define wren_ast_h

#include "wren_parser.h"
#include "wren_compiler_ops.h"

// forward declarations
typedef struct Expr_t Expr;
typedef struct ExprNode ExprNode;
typedef struct Stmt_t Stmt;
typedef struct StmtNode StmtNode;
typedef struct Body Body;
typedef struct MethodNode_t MethodNode;

typedef struct TokenNode {
  Token val;
  struct TokenNode* next;
} TokenNode;

typedef struct Alias {
  Token val;
  Token alias;
} Alias;

typedef struct AliasNode {
	Alias val;
	struct AliasNode* next;
} AliasNode;

typedef struct Expr_t {
  enum { ListExpr, ThisExpr, NullExpr, VarExpr,
         StaticFieldExpr, FieldExpr, CallExpr,
         PrefixExpr, GroupingExpr, AssignmentExpr,
         InfixExpr, MapExpr, ConditionalExpr,
         NumExpr, SuperExpr, StringExpr,
         SubscriptExpr, BoolExpr, InterpolationExpr } tag;
  union { Token                                       tokenExpr;
          struct { Token             leftBracket;
                   Token             rightBracket;
                   ExprNode*         elements; }      listExpr;
          struct {
                   Expr*             receiver;
                   Token             name;
                   ExprNode*         arguments;
                   Body*             blockArgument; } callExpr;
          struct { Token             operator;
                   Expr*             operand; }       prefixExpr;
          struct {
                   Token             leftParen;
                   Token             rightParen;
                   Expr*             expression; }    groupingExpr;
          struct {
                   Expr*             target;
                   Token             equals;
                   Expr*             value; }         assignmentExpr;
          struct {
                   Expr*             left;
                   Token             operator;
                   Expr*             right; }         infixExpr;
          struct {
                   Token             leftBrace;
                   ExprNode*         keys;
                   ExprNode*         values;
                   Token             rightBrace; }    mapExpr;
          struct {
                   Expr*             condition;
                   Token             question;
                   Expr*             thenBranch;
                   Token             colon;
                   Expr*             elseBranch; }    conditionalExpr;
          struct {
                   Token             name;
                   ExprNode*         arguments;
                   Body*             blockArgument; } superExpr;
          struct {
                   Expr*             receiver;
                   Token             leftBracket;
                   struct ExprNode*  args;
                   Token             rightBracket; }  subscriptExpr;
          struct {
                   ExprNode*         strings;
                   ExprNode*         expressions; }   interpolationExpr;
      } op;
} Expr;

typedef struct ExprNode {
  Expr* val;
  struct ExprNode* next;
} ExprNode;

// Statement (Stmt)
typedef struct Stmt_t {
  enum { ForStmt, ReturnStmt, BlockStmt,
         VarStmt, ImportStmt, IfStmt,
         BreakStmt, ContinueStmt, WhileStmt,
         ClassStmt, ExprStmt }                        tag;
  union { struct {
                   Token             variable;
                   Expr*             iterator;
                   Stmt*             body; }          forStmt;
          struct {
                   Token             keyword;
                   Expr*             value; }         returnStmt;
          struct {
                   StmtNode*         statements; }    blockStmt;
          struct {
                   Token             name;
                   Expr*             initializer; }   varStmt;
          struct {
                   Token             path;
                   AliasNode*        variables; }     importStmt;
          struct {
                   Expr*             condition;
                   Stmt*             thenBranch;
                   Stmt*             elseBranch; }    ifStmt;
          struct {
                   Token             keyword; }       breakStmt;
          struct {
                   Token             keyword; }       continueStmt;
          struct {
                   Expr*             condition;
                   Stmt*             body; }          whileStmt;
          struct {
                   bool              foreign;
                   Token             name;
                   Expr*             superclass;
                   MethodNode*       methods; }       classStmt;
          struct {
                   Expr*             expression; }    exprStmt;
      } op;
} Stmt;

typedef struct StmtNode {
  Stmt* val;
  struct StmtNode* next;
} StmtNode;

typedef struct {
  bool isForeign;
  bool isStatic;
  bool isConstruct;
  Token name;
  Body* body;
  TokenNode* parameters;
} Method_t;

typedef struct MethodNode_t {
  Method_t* val;
  MethodNode* next;
} MethodNode;

typedef struct {
  TokenNode* Parameters;
  Expr* expression;
  StmtNode* statements;
} Body_t;

typedef struct astCompiler
{
  CompilerBase base;

  // The compiler for the function enclosing this one, or NULL if it's the
  // top level.
  CompilerBase* parent;

  // Linked list of all statements (new entries are added to the front)
  StmtNode* statements;

  // Statement stack - used to push/pop statements during ast compilation
  StmtNode* statementStack;

  // Linked list of all expressions (new entries are added to the front)
  ExprNode* expressions;

  // Semantic stack - used to push/pop expressions during ast compilation
  ExprNode* semanticStack;

} astCompiler;

#endif
