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

typedef struct CompilerBase_t
{
  Parser* parser;
  void* derived;
  const struct CompilerOps* ops;

  // The compiler for the function enclosing this one, or NULL if it's the
  // top level.
  CompilerBase* parent;

  // The current level of block scope nesting, where zero is no nesting. A -1
  // here means top-level code is being compiled and there is no block scope
  // in effect at all. Any variables declared will be module-level.
  int scopeDepth;

  // The current number of slots (locals and temporaries) in use.
  //
  // We use this and maxSlots to track the maximum number of additional slots
  // a function may need while executing. When the function is called, the
  // fiber will check to ensure its stack has enough room to cover that worst
  // case and grow the stack if needed.
  //
  // This value here doesn't include parameters to the function. Since those
  // are already pushed onto the stack by the caller and tracked there, we
  // don't need to double count them here.
  int numSlots;

  // The number of local variables currently in scope.
  int numLocals;

  // The currently in scope local variables.
  Local locals[MAX_LOCALS];
} CompilerBase;

struct CompilerOps
{
  void (*classDefinition)(CompilerBase* compiler, bool isForeign);
  void (*variableDefinition)(CompilerBase* compiler);
  void (*attributeDefinition)(CompilerBase* compiler, bool runtimeAccess, Value group, Value key, Value value);
  void (*import)(CompilerBase* compiler);
  void (*breakStatement)(CompilerBase* compiler);
  void (*continueStatement)(CompilerBase* compiler);
  void (*forStatement)(CompilerBase* compiler);
  void (*ifStatement)(CompilerBase* compiler);
  void (*returnStatement)(CompilerBase* compiler);
  void (*whileStatement)(CompilerBase* compiler);
  void (*blockStatement)(CompilerBase* compiler);
  void (*expressionStatement)(CompilerBase* compiler);

  void (*discardAttributes)(CompilerBase* compiler);
  int (*discardLocals)(CompilerBase* compiler, int depth);
  int (*defineModuleVariable)(CompilerBase* compiler, Token* token);

  void (*list)(CompilerBase* compiler, bool canAssign);
  void (*subscript)(CompilerBase* compiler, bool canAssign);
  void (*map)(CompilerBase* compiler, bool canAssign);
  void (*call)(CompilerBase* compiler, bool canAssign);
  void (*infixOp)(CompilerBase* compiler, bool canAssign);
  void (*unaryOp)(CompilerBase* compiler, bool canAssign);
  void (*or_)(CompilerBase* compiler, bool canAssign);
  void (*and_)(CompilerBase* compiler, bool canAssign);
  void (*conditional)(CompilerBase* compiler, bool canAssign);
  void (*boolean)(CompilerBase* compiler, bool canAssign);
  void (*null)(CompilerBase* compiler, bool canAssign);
  void (*super_)(CompilerBase* compiler, bool canAssign);
  void (*this_)(CompilerBase* compiler, bool canAssign);
  void (*field)(CompilerBase* compiler, bool canAssign);
  void (*staticField)(CompilerBase* compiler, bool canAssign);
  void (*name)(CompilerBase* compiler, bool canAssign);
  void (*literal)(CompilerBase* compiler, bool canAssign);
  void (*stringInterpolation)(CompilerBase* compiler, bool canAssign);
};

#endif
