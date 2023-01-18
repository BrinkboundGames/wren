#ifndef wren_compiler_ops_h
#define wren_compiler_ops_h

#include "wren.h"
#include "wren_value.h"
#include "wren_parser.h"

// This is written in bottom-up order, so the tokenization comes first, then
// parsing/code generation. This minimizes the number of explicit forward
// declarations needed.

// The maximum number of local (i.e. not module level) variables that can be
// declared in a single function, method, or chunk of top level code. This is
// the maximum number of variables in scope at one time, and spans block scopes.
//
// Note that this limitation is also explicit in the bytecode. Since
// `CODE_LOAD_LOCAL` and `CODE_STORE_LOCAL` use a single argument byte to
// identify the local, only 256 can be in scope at one time.
#define MAX_LOCALS 256

typedef struct
{
  // The name of the local variable. This points directly into the original
  // source code string.
  const char* name;

  // The length of the local variable's name.
  int length;

  // The depth in the scope chain that this variable was declared at. Zero is
  // the outermost scope--parameters for a method, or the first local block in
  // top level code. One is the scope within that, etc.
  int depth;

  // If this local variable is being used as an upvalue.
  bool isUpvalue;
} Local;

struct CompilerOps
{
  void (*classDefinition)(struct CompilerBase* compiler, bool isForeign);
  void (*variableDefinition)(struct CompilerBase* compiler);
  void (*attributeDefinition)(struct CompilerBase* compiler, bool runtimeAccess, Value group, Value key, Value value);
  void (*import)(struct CompilerBase* compiler);
  void (*breakStatement)(struct CompilerBase* compiler);
  void (*continueStatement)(struct CompilerBase* compiler);
  void (*forStatement)(struct CompilerBase* compiler);
  void (*ifStatement)(struct CompilerBase* compiler);
  void (*returnStatement)(struct CompilerBase* compiler);
  void (*whileStatement)(struct CompilerBase* compiler);
  void (*blockStatement)(struct CompilerBase* compiler);
  void (*expressionStatement)(struct CompilerBase* compiler);

  void (*discardAttributes)(struct CompilerBase* compiler);
  int (*defineModuleVariable)(struct CompilerBase* compiler, Token* token);

  void (*list)(struct CompilerBase* compiler, bool canAssign);
  void (*subscript)(struct CompilerBase* compiler, bool canAssign);
  void (*map)(struct CompilerBase* compiler, bool canAssign);
  void (*call)(struct CompilerBase* compiler, bool canAssign);
  void (*infixOp)(struct CompilerBase* compiler, bool canAssign);
  void (*unaryOp)(struct CompilerBase* compiler, bool canAssign);
  void (*or_)(struct CompilerBase* compiler, bool canAssign);
  void (*and_)(struct CompilerBase* compiler, bool canAssign);
  void (*conditional)(struct CompilerBase* compiler, bool canAssign);
  void (*boolean)(struct CompilerBase* compiler, bool canAssign);
  void (*null)(struct CompilerBase* compiler, bool canAssign);
  void (*super_)(struct CompilerBase* compiler, bool canAssign);
  void (*this_)(struct CompilerBase* compiler, bool canAssign);
  void (*field)(struct CompilerBase* compiler, bool canAssign);
  void (*staticField)(struct CompilerBase* compiler, bool canAssign);
  void (*name)(struct CompilerBase* compiler, bool canAssign);
  void (*literal)(struct CompilerBase* compiler, bool canAssign);
  void (*stringInterpolation)(struct CompilerBase* compiler, bool canAssign);
};

typedef struct
{
  Parser* parser;
  void* derived;
  const struct CompilerOps* ops;

  // The current level of block scope nesting, where zero is no nesting. A -1
  // here means top-level code is being compiled and there is no block scope
  // in effect at all. Any variables declared will be module-level.
  int scopeDepth;

  // The number of local variables currently in scope.
  int numLocals;

  // The currently in scope local variables.
  Local locals[MAX_LOCALS];
} CompilerBase;

#endif
