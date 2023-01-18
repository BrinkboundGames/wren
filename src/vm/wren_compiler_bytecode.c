#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#include "wren_common.h"
#include "wren_compiler_bytecode.h"
#include "wren_vm.h"
#include "wren_parser.h"
#include "wren_compiler_ops.h"

#if WREN_DEBUG_DUMP_COMPILED_CODE
  #include "wren_debug.h"
#endif

// The maximum number of upvalues (i.e. variables from enclosing functions)
// that a function can close over.
#define MAX_UPVALUES 256

// The maximum number of distinct constants that a function can contain. This
// value is explicit in the bytecode since `CODE_CONSTANT` only takes a single
// two-byte argument.
#define MAX_CONSTANTS (1 << 16)

// The maximum distance a CODE_JUMP or CODE_JUMP_IF instruction can move the
// instruction pointer.
#define MAX_JUMP (1 << 16)

#define GET_PARENT(compiler) ((Compiler*)(compiler->base.parent ? compiler->base.parent->derived : NULL))

typedef struct
{
  // True if this upvalue is capturing a local variable from the enclosing
  // function. False if it's capturing an upvalue.
  bool isLocal;

  // The index of the local or upvalue being captured in the enclosing function.
  int index;
} CompilerUpvalue;

// Bookkeeping information for the current loop being compiled.
typedef struct sLoop
{
  // Index of the instruction that the loop should jump back to.
  int start;

  // Index of the argument for the CODE_JUMP_IF instruction used to exit the
  // loop. Stored so we can patch it once we know where the loop ends.
  int exitJump;

  // Index of the first instruction of the body of the loop.
  int body;

  // Depth of the scope(s) that need to be exited if a break is hit inside the
  // loop.
  int scopeDepth;

  // The loop enclosing this one, or NULL if this is the outermost loop.
  struct sLoop* enclosing;
} Loop;

// Bookkeeping information for compiling a class definition.
typedef struct
{
  // The name of the class.
  ObjString* name;

  // Attributes for the class itself
  ObjMap* classAttributes;
  // Attributes for methods in this class
  ObjMap* methodAttributes;

  // Symbol table for the fields of the class.
  SymbolTable fields;

  // Symbols for the methods defined by the class. Used to detect duplicate
  // method definitions.
  IntBuffer methods;
  IntBuffer staticMethods;

  // True if the class being compiled is a foreign class.
  bool isForeign;

  // True if the current method being compiled is static.
  bool inStatic;

  // The signature of the method being compiled.
  Signature* signature;
} ClassInfo;

struct sCompiler
{
  CompilerBase base;

  // The upvalues that this function has captured from outer scopes. The count
  // of them is stored in [numUpvalues].
  CompilerUpvalue upvalues[MAX_UPVALUES];

  // The current innermost loop being compiled, or NULL if not in a loop.
  Loop* loop;

  // If this is a compiler for a method, keeps track of the class enclosing it.
  ClassInfo* enclosingClass;

  // The module being compiled.
  ObjModule* module;

  // The function being compiled.
  ObjFn* fn;

  // The constants for the function being compiled.
  ObjMap* constants;

  // Whether or not the compiler is for a constructor initializer
  bool isInitializer;

  // Attributes for the next class or method.
  ObjMap* attributes;
};

// Describes where a variable is declared.
typedef enum
{
  // A local variable in the current function.
  SCOPE_LOCAL,

  // A local variable declared in an enclosing function.
  SCOPE_UPVALUE,

  // A top-level module variable.
  SCOPE_MODULE
} Scope;

// A reference to a variable and the scope where it is defined. This contains
// enough information to emit correct code to load or store the variable.
typedef struct
{
  // The stack slot, upvalue slot, or module symbol defining the variable.
  int index;
  
  // Where the variable is declared.
  Scope scope;
} Variable;

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
static int discardLocals(CompilerBase* compiler, int depth);
static int defineModuleVariable(CompilerBase* compiler, Token* token);
static void list(CompilerBase* compiler, bool canAssign);
static void subscript(CompilerBase* compiler, bool canAssign);
static void map(CompilerBase* compiler, bool canAssign);
static void call(CompilerBase* compiler, bool canAssign);
static void infixOp(CompilerBase* compiler, bool canAssign);
static void unaryOp(CompilerBase* compiler, bool canAssign);
static void or_(CompilerBase* compiler, bool canAssign);
static void and_(CompilerBase* compiler, bool canAssign);
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

static void emitClassAttributes(Compiler* compiler, ClassInfo* classInfo);
static void copyAttributes(Compiler* compiler, ObjMap* into);
static void copyMethodAttributes(Compiler* compiler, bool isForeign, 
            bool isStatic, const char* fullSignature, int32_t length);

// The stack effect of each opcode. The index in the array is the opcode, and
// the value is the stack effect of that instruction.
static const int stackEffects[] = {
  #define OPCODE(_, effect) effect,
  #include "wren_opcodes.h"
  #undef OPCODE
};

// Adds [constant] to the constant pool and returns its index.
static int addConstant(Compiler* compiler, Value constant)
{
  if (compiler->base.parser->hasError) return -1;

  // See if we already have a constant for the value. If so, reuse it.
  if (compiler->constants != NULL)
  {
    Value existing = wrenMapGet(compiler->constants, constant);
    if (IS_NUM(existing)) return (int)AS_NUM(existing);
  }

  // It's a new constant.
  if (compiler->fn->constants.count < MAX_CONSTANTS)
  {
    if (IS_OBJ(constant)) wrenPushRoot(compiler->base.parser->vm, AS_OBJ(constant));
    wrenValueBufferWrite(compiler->base.parser->vm, &compiler->fn->constants,
                         constant);
    if (IS_OBJ(constant)) wrenPopRoot(compiler->base.parser->vm);

    if (compiler->constants == NULL)
    {
      compiler->constants = wrenNewMap(compiler->base.parser->vm);
    }
    wrenMapSet(compiler->base.parser->vm, compiler->constants, constant,
               NUM_VAL(compiler->fn->constants.count - 1));
  }
  else
  {
    error(compiler->base.parser, "A function may only contain %d unique constants.",
          MAX_CONSTANTS);
  }

  return compiler->fn->constants.count - 1;
}

// Initializes [compiler].
static void initCompiler(Compiler* compiler, Parser* parser, ObjModule* module, Compiler* parent,
                         bool isMethod)
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
    discardLocals,
    defineModuleVariable,
    list,
    subscript,
    map,
    call,
    infixOp,
    unaryOp,
    or_,
    and_,
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
  compiler->base.parent = parent ? &parent->base : NULL;

  compiler->module = module;
  compiler->loop = NULL;
  compiler->enclosingClass = NULL;
  compiler->isInitializer = false;

  // Initialize these to NULL before allocating in case a GC gets triggered in
  // the middle of initializing the compiler.
  compiler->fn = NULL;
  compiler->constants = NULL;
  compiler->attributes = NULL;

  parser->vm->compiler = compiler;

  // Declare a local slot for either the closure or method receiver so that we
  // don't try to reuse that slot for a user-defined local variable. For
  // methods, we name it "this", so that we can resolve references to that like
  // a normal variable. For functions, they have no explicit "this", so we use
  // an empty name. That way references to "this" inside a function walks up
  // the parent chain to find a method enclosing the function whose "this" we
  // can close over.
  compiler->base.numLocals = 1;
  compiler->base.numSlots = compiler->base.numLocals;

  if (isMethod)
  {
    compiler->base.locals[0].name = "this";
    compiler->base.locals[0].length = 4;
  }
  else
  {
    compiler->base.locals[0].name = NULL;
    compiler->base.locals[0].length = 0;
  }

  compiler->base.locals[0].depth = -1;
  compiler->base.locals[0].isUpvalue = false;

  if (parent == NULL)
  {
    // Compiling top-level code, so the initial scope is module-level.
    compiler->base.scopeDepth = -1;
  }
  else
  {
    // The initial scope for functions and methods is local scope.
    compiler->base.scopeDepth = 0;
  }

  compiler->attributes = wrenNewMap(parser->vm);
  compiler->fn = wrenNewFunction(parser->vm, module,
                                 compiler->base.numLocals);
}

// Variables and scopes --------------------------------------------------------

// Emits one single-byte argument. Returns its index.
static int emitByte(Compiler* compiler, int byte)
{
  wrenByteBufferWrite(compiler->base.parser->vm, &compiler->fn->code, (uint8_t)byte);
  
  // Assume the instruction is associated with the most recently consumed token.
  wrenIntBufferWrite(compiler->base.parser->vm, &compiler->fn->debug->sourceLines,
                     compiler->base.parser->previous.line);
  
  return compiler->fn->code.count - 1;
}

// Emits one bytecode instruction.
static void emitOp(Compiler* compiler, Code instruction)
{
  emitByte(compiler, instruction);
  
  // Keep track of the stack's high water mark.
  compiler->base.numSlots += stackEffects[instruction];
  if (compiler->base.numSlots > compiler->fn->maxSlots)
  {
    compiler->fn->maxSlots = compiler->base.numSlots;
  }
}

// Emits one 16-bit argument, which will be written big endian.
static void emitShort(Compiler* compiler, int arg)
{
  emitByte(compiler, (arg >> 8) & 0xff);
  emitByte(compiler, arg & 0xff);
}

// Emits one bytecode instruction followed by a 8-bit argument. Returns the
// index of the argument in the bytecode.
static int emitByteArg(Compiler* compiler, Code instruction, int arg)
{
  emitOp(compiler, instruction);
  return emitByte(compiler, arg);
}

// Emits one bytecode instruction followed by a 16-bit argument, which will be
// written big endian.
static void emitShortArg(Compiler* compiler, Code instruction, int arg)
{
  emitOp(compiler, instruction);
  emitShort(compiler, arg);
}

// Emits [instruction] followed by a placeholder for a jump offset. The
// placeholder can be patched by calling [jumpPatch]. Returns the index of the
// placeholder.
static int emitJump(Compiler* compiler, Code instruction)
{
  emitOp(compiler, instruction);
  emitByte(compiler, 0xff);
  return emitByte(compiler, 0xff) - 1;
}

// Creates a new constant for the current value and emits the bytecode to load
// it from the constant table.
static void emitConstant(Compiler* compiler, Value value)
{
  int constant = addConstant(compiler, value);

  // Compile the code to load the constant.
  emitShortArg(compiler, CODE_CONSTANT, constant);
}

static int defineModuleVariable(CompilerBase* compiler, Token* token)
{
  Compiler* derived = compiler->derived;
  int line = -1;
  int symbol = wrenDefineVariable(compiler->parser->vm,
    derived->module,
    token->start, token->length,
    NULL_VAL, &line);

  if (symbol == -1)
  {
    error(compiler->parser, "Module variable is already defined.");
  }
  else if (symbol == -2)
  {
    error(compiler->parser, "Too many module variables defined.");
  }
  else if (symbol == -3)
  {
    error(compiler->parser,
      "Variable '%.*s' referenced before this definition (first use at line %d).",
      token->length, token->start, line);
  }

  return symbol;
}

// Stores a variable with the previously defined symbol in the current scope.
static void defineVariable(Compiler* compiler, int symbol)
{
  // Store the variable. If it's a local, the result of the initializer is
  // in the correct slot on the stack already so we're done.
  if (compiler->base.scopeDepth >= 0) return;

  // It's a module-level variable, so store the value in the module slot and
  // then discard the temporary for the initializer.
  emitShortArg(compiler, CODE_STORE_MODULE_VAR, symbol);
  emitOp(compiler, CODE_POP);
}

// Generates code to discard local variables at [depth] or greater. Does *not*
// actually undeclare variables or pop any scopes, though. This is called
// directly when compiling "break" statements to ditch the local variables
// before jumping out of the loop even though they are still in scope *past*
// the break instruction.
//
// Returns the number of local variables that were eliminated.
static int discardLocals(CompilerBase* compiler, int depth)
{
  ASSERT(compiler->scopeDepth > -1, "Cannot exit top-level scope.");

  Compiler* derived = compiler->derived;
  int local = compiler->numLocals - 1;
  while (local >= 0 && compiler->locals[local].depth >= depth)
  {
    // If the local was closed over, make sure the upvalue gets closed when it
    // goes out of scope on the stack. We use emitByte() and not emitOp() here
    // because we don't want to track that stack effect of these pops since the
    // variables are still in scope after the break.
    if (compiler->locals[local].isUpvalue)
    {
      emitByte(derived, CODE_CLOSE_UPVALUE);
    }
    else
    {
      emitByte(derived, CODE_POP);
    }

    local--;
  }

  return compiler->numLocals - local - 1;
}

// Adds an upvalue to [compiler]'s function with the given properties. Does not
// add one if an upvalue for that variable is already in the list. Returns the
// index of the upvalue.
static int addUpvalue(Compiler* compiler, bool isLocal, int index)
{
  // Look for an existing one.
  for (int i = 0; i < compiler->fn->numUpvalues; i++)
  {
    CompilerUpvalue* upvalue = &compiler->upvalues[i];
    if (upvalue->index == index && upvalue->isLocal == isLocal) return i;
  }

  // If we got here, it's a new upvalue.
  compiler->upvalues[compiler->fn->numUpvalues].isLocal = isLocal;
  compiler->upvalues[compiler->fn->numUpvalues].index = index;
  return compiler->fn->numUpvalues++;
}

// Attempts to look up [name] in the functions enclosing the one being compiled
// by [compiler]. If found, it adds an upvalue for it to this compiler's list
// of upvalues (unless it's already in there) and returns its index. If not
// found, returns -1.
//
// If the name is found outside of the immediately enclosing function, this
// will flatten the closure and add upvalues to all of the intermediate
// functions so that it gets walked down to this one.
//
// If it reaches a method boundary, this stops and returns -1 since methods do
// not close over local variables.
static int findUpvalue(Compiler* compiler, const char* name, int length)
{
  // If we are at the top level, we didn't find it.
  if (compiler->base.parent == NULL) return -1;

  // If we hit the method boundary (and the name isn't a static field), then
  // stop looking for it. We'll instead treat it as a self send.
  if (name[0] != '_' && GET_PARENT(compiler)->enclosingClass != NULL) return -1;

  // See if it's a local variable in the immediately enclosing function.
  int local = resolveLocal(&GET_PARENT(compiler)->base, name, length);
  if (local != -1)
  {
    // Mark the local as an upvalue so we know to close it when it goes out of
    // scope.
    GET_PARENT(compiler)->base.locals[local].isUpvalue = true;

    return addUpvalue(compiler, true, local);
  }

  // See if it's an upvalue in the immediately enclosing function. In other
  // words, if it's a local variable in a non-immediately enclosing function.
  // This "flattens" closures automatically: it adds upvalues to all of the
  // intermediate functions to get from the function where a local is declared
  // all the way into the possibly deeply nested function that is closing over
  // it.
  int upvalue = findUpvalue(GET_PARENT(compiler), name, length);
  if (upvalue != -1)
  {
    return addUpvalue(compiler, false, upvalue);
  }

  // If we got here, we walked all the way up the parent chain and couldn't
  // find it.
  return -1;
}

// Look up [name] in the current scope to see what variable it refers to.
// Returns the variable either in local scope, or the enclosing function's
// upvalue list. Does not search the module scope. Returns a variable with
// index -1 if not found.
static Variable resolveNonmodule(Compiler* compiler,
                                 const char* name, int length)
{
  // Look it up in the local scopes.
  Variable variable;
  variable.scope = SCOPE_LOCAL;
  variable.index = resolveLocal(&compiler->base, name, length);
  if (variable.index != -1) return variable;

  // It's not a local, so guess that it's an upvalue.
  variable.scope = SCOPE_UPVALUE;
  variable.index = findUpvalue(compiler, name, length);
  return variable;
}

// Look up [name] in the current scope to see what variable it refers to.
// Returns the variable either in module scope, local scope, or the enclosing
// function's upvalue list. Returns a variable with index -1 if not found.
static Variable resolveName(Compiler* compiler, const char* name, int length)
{
  Variable variable = resolveNonmodule(compiler, name, length);
  if (variable.index != -1) return variable;

  variable.scope = SCOPE_MODULE;
  variable.index = wrenSymbolTableFind(&compiler->module->variableNames,
                                       name, length);
  return variable;
}

static void loadLocal(Compiler* compiler, int slot)
{
  if (slot <= 8)
  {
    emitOp(compiler, (Code)(CODE_LOAD_LOCAL_0 + slot));
    return;
  }

  emitByteArg(compiler, CODE_LOAD_LOCAL, slot);
}

// Finishes [compiler], which is compiling a function, method, or chunk of top
// level code. If there is a parent compiler, then this emits code in the
// parent compiler to load the resulting function.
static ObjFn* endCompiler(Compiler* compiler,
                          const char* debugName, int debugNameLength)
{
  // If we hit an error, don't finish the function since it's borked anyway.
  if (compiler->base.parser->hasError)
  {
    compiler->base.parser->vm->compiler = GET_PARENT(compiler);
    return NULL;
  }

  // Mark the end of the bytecode. Since it may contain multiple early returns,
  // we can't rely on CODE_RETURN to tell us we're at the end.
  emitOp(compiler, CODE_END);

  wrenFunctionBindName(compiler->base.parser->vm, compiler->fn,
                       debugName, debugNameLength);
  
  // In the function that contains this one, load the resulting function object.
  if (compiler->base.parent != NULL)
  {
    int constant = addConstant(GET_PARENT(compiler), OBJ_VAL(compiler->fn));

    // Wrap the function in a closure. We do this even if it has no upvalues so
    // that the VM can uniformly assume all called objects are closures. This
    // makes creating a function a little slower, but makes invoking them
    // faster. Given that functions are invoked more often than they are
    // created, this is a win.
    emitShortArg(GET_PARENT(compiler), CODE_CLOSURE, constant);

    // Emit arguments for each upvalue to know whether to capture a local or
    // an upvalue.
    for (int i = 0; i < compiler->fn->numUpvalues; i++)
    {
      emitByte(GET_PARENT(compiler), compiler->upvalues[i].isLocal ? 1 : 0);
      emitByte(GET_PARENT(compiler), compiler->upvalues[i].index);
    }
  }

  // Pop this compiler off the stack.
  compiler->base.parser->vm->compiler = GET_PARENT(compiler);
  
  #if WREN_DEBUG_DUMP_COMPILED_CODE
    wrenDumpCode(compiler->base.parser->vm, compiler->fn);
  #endif

  return compiler->fn;
}

// Replaces the placeholder argument for a previous CODE_JUMP or CODE_JUMP_IF
// instruction with an offset that jumps to the current end of bytecode.
static void patchJump(Compiler* compiler, int offset)
{
  // -2 to adjust for the bytecode for the jump offset itself.
  int jump = compiler->fn->code.count - offset - 2;
  if (jump > MAX_JUMP) error(compiler->base.parser, "Too much code to jump over.");

  compiler->fn->code.data[offset] = (jump >> 8) & 0xff;
  compiler->fn->code.data[offset + 1] = jump & 0xff;
}

// Parses a method or function body, after the initial "{" has been consumed.
//
// If [Compiler->isInitializer] is `true`, this is the body of a constructor
// initializer. In that case, this adds the code to ensure it returns `this`.
static void finishBody(Compiler* compiler)
{
  bool isExpressionBody = finishBlock(&compiler->base);

  if (compiler->isInitializer)
  {
    // If the initializer body evaluates to a value, discard it.
    if (isExpressionBody) emitOp(compiler, CODE_POP);

    // The receiver is always stored in the first local slot.
    emitOp(compiler, CODE_LOAD_LOCAL_0);
  }
  else if (!isExpressionBody)
  {
    // Implicitly return null in statement bodies.
    emitOp(compiler, CODE_NULL);
  }

  emitOp(compiler, CODE_RETURN);
}

// Gets the symbol for a method [name] with [length].
static int methodSymbol(Compiler* compiler, const char* name, int length)
{
  return wrenSymbolTableEnsure(compiler->base.parser->vm,
      &compiler->base.parser->vm->methodNames, name, length);
}

// Appends characters to [name] (and updates [length]) for [numParams] "_"
// surrounded by [leftBracket] and [rightBracket].
static void signatureParameterList(char name[MAX_METHOD_SIGNATURE], int* length,
                                   int numParams, char leftBracket, char rightBracket)
{
  name[(*length)++] = leftBracket;

  // This function may be called with too many parameters. When that happens,
  // a compile error has already been reported, but we need to make sure we
  // don't overflow the string too, hence the MAX_PARAMETERS check.
  for (int i = 0; i < numParams && i < MAX_PARAMETERS; i++)
  {
    if (i > 0) name[(*length)++] = ',';
    name[(*length)++] = '_';
  }
  name[(*length)++] = rightBracket;
}

// Fills [name] with the stringified version of [signature] and updates
// [length] to the resulting length.
static void signatureToString(Signature* signature,
                              char name[MAX_METHOD_SIGNATURE], int* length)
{
  *length = 0;

  // Build the full name from the signature.
  memcpy(name + *length, signature->name, signature->length);
  *length += signature->length;

  switch (signature->type)
  {
    case SIG_METHOD:
      signatureParameterList(name, length, signature->arity, '(', ')');
      break;

    case SIG_GETTER:
      // The signature is just the name.
      break;

    case SIG_SETTER:
      name[(*length)++] = '=';
      signatureParameterList(name, length, 1, '(', ')');
      break;

    case SIG_SUBSCRIPT:
      signatureParameterList(name, length, signature->arity, '[', ']');
      break;

    case SIG_SUBSCRIPT_SETTER:
      signatureParameterList(name, length, signature->arity - 1, '[', ']');
      name[(*length)++] = '=';
      signatureParameterList(name, length, 1, '(', ')');
      break;
      
    case SIG_INITIALIZER:
      memcpy(name, "init ", 5);
      memcpy(name + 5, signature->name, signature->length);
      *length = 5 + signature->length;
      signatureParameterList(name, length, signature->arity, '(', ')');
      break;
  }

  name[*length] = '\0';
}

// Gets the symbol for a method with [signature].
static int signatureSymbol(Compiler* compiler, Signature* signature)
{
  // Build the full name from the signature.
  char name[MAX_METHOD_SIGNATURE];
  int length;
  signatureToString(signature, name, &length);

  return methodSymbol(compiler, name, length);
}

// Compiles a method call with [signature] using [instruction].
static void callSignature(Compiler* compiler, Code instruction,
                          Signature* signature)
{
  int symbol = signatureSymbol(compiler, signature);
  emitShortArg(compiler, (Code)(instruction + signature->arity), symbol);

  if (instruction == CODE_SUPER_0)
  {
    // Super calls need to be statically bound to the class's superclass. This
    // ensures we call the right method even when a method containing a super
    // call is inherited by another subclass.
    //
    // We bind it at class definition time by storing a reference to the
    // superclass in a constant. So, here, we create a slot in the constant
    // table and store NULL in it. When the method is bound, we'll look up the
    // superclass then and store it in the constant slot.
    emitShort(compiler, addConstant(compiler, NULL_VAL));
  }
}

// Compiles a method call with [numArgs] for a method with [name] with [length].
static void callMethod(Compiler* compiler, int numArgs, const char* name,
                       int length)
{
  int symbol = methodSymbol(compiler, name, length);
  emitShortArg(compiler, (Code)(CODE_CALL_0 + numArgs), symbol);
}

// Compiles an (optional) argument list for a method call with [signature]
// and then calls it.
static void methodCall(Compiler* compiler, Code instruction,
                       Signature* signature)
{
  // Make a new signature that contains the updated arity and type based on
  // the arguments we find.
  Signature called = { signature->name, signature->length, SIG_GETTER, 0 };

  // Parse the argument list, if any.
  if (match(compiler->base.parser, TOKEN_LEFT_PAREN))
  {
    called.type = SIG_METHOD;

    // Allow new line before an empty argument list
    ignoreNewlines(compiler->base.parser);

    // Allow empty an argument list.
    if (peek(compiler->base.parser) != TOKEN_RIGHT_PAREN)
    {
      finishArgumentList(&compiler->base, &called.arity);
    }
    consume(compiler->base.parser, TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
  }

  // Parse the block argument, if any.
  if (match(compiler->base.parser, TOKEN_LEFT_BRACE))
  {
    // Include the block argument in the arity.
    called.type = SIG_METHOD;
    called.arity++;

    Compiler fnCompiler;
    initCompiler(&fnCompiler, compiler->base.parser, compiler->module, compiler, false);

    // Track the arity.
    int arity = 0;

    // Parse the parameter list, if any.
    if (match(compiler->base.parser, TOKEN_PIPE))
    {
      finishParameterList(&fnCompiler.base, &arity);
      consume(compiler->base.parser, TOKEN_PIPE, "Expect '|' after function parameters.");
    }

    fnCompiler.fn->arity = arity;

    finishBody(&fnCompiler);

    // Name the function based on the method its passed to.
    char blockName[MAX_METHOD_SIGNATURE + 15];
    int blockLength;
    signatureToString(&called, blockName, &blockLength);
    memmove(blockName + blockLength, " block argument", 16);

    endCompiler(&fnCompiler, blockName, blockLength + 15);
  }

  // TODO: Allow Grace-style mixfix methods?

  // If this is a super() call for an initializer, make sure we got an actual
  // argument list.
  if (signature->type == SIG_INITIALIZER)
  {
    if (called.type != SIG_METHOD)
    {
      error(compiler->base.parser, "A superclass constructor must have an argument list.");
    }

    called.type = SIG_INITIALIZER;
  }

  callSignature(compiler, instruction, &called);
}

// Compiles a call whose name is the previously consumed token. This includes
// getters, method calls with arguments, and setter calls.
static void namedCall(Compiler* compiler, bool canAssign, Code instruction)
{
  // Get the token for the method name.
  Signature signature = signatureFromToken(compiler->base.parser, SIG_GETTER);

  if (canAssign && match(compiler->base.parser, TOKEN_EQ))
  {
    ignoreNewlines(compiler->base.parser);

    // Build the setter signature.
    signature.type = SIG_SETTER;
    signature.arity = 1;

    // Compile the assigned value.
    expression(&compiler->base);
    callSignature(compiler, instruction, &signature);
  }
  else
  {
    methodCall(compiler, instruction, &signature);
    allowLineBeforeDot(compiler->base.parser);
  }
}

// Emits the code to load [variable] onto the stack.
static void loadVariable(Compiler* compiler, Variable variable)
{
  switch (variable.scope)
  {
    case SCOPE_LOCAL:
      loadLocal(compiler, variable.index);
      break;
    case SCOPE_UPVALUE:
      emitByteArg(compiler, CODE_LOAD_UPVALUE, variable.index);
      break;
    case SCOPE_MODULE:
      emitShortArg(compiler, CODE_LOAD_MODULE_VAR, variable.index);
      break;
    default:
      UNREACHABLE();
  }
}

// Loads the receiver of the currently enclosing method. Correctly handles
// functions defined inside methods.
static void loadThis(Compiler* compiler)
{
  loadVariable(compiler, resolveNonmodule(compiler, "this", 4));
}

// Pushes the value for a module-level variable implicitly imported from core.
static void loadCoreVariable(Compiler* compiler, const char* name)
{
  int symbol = wrenSymbolTableFind(&compiler->module->variableNames,
                                   name, strlen(name));
  ASSERT(symbol != -1, "Should have already defined core name.");
  emitShortArg(compiler, CODE_LOAD_MODULE_VAR, symbol);
}

// A list literal.
static void list(CompilerBase* compiler, bool canAssign)
{
  Compiler* derived = compiler->derived;

  // Instantiate a new list.
  loadCoreVariable(derived, "List");
  callMethod(derived, 0, "new()", 5);

  // Compile the list elements. Each one compiles to a ".add()" call.
  do
  {
    ignoreNewlines(compiler->parser);

    // Stop if we hit the end of the list.
    if (peek(compiler->parser) == TOKEN_RIGHT_BRACKET) break;

    // The element.
    expression(compiler);
    callMethod(derived, 1, "addCore_(_)", 11);
  } while (match(compiler->parser, TOKEN_COMMA));

  // Allow newlines before the closing ']'.
  ignoreNewlines(compiler->parser);
  consume(compiler->parser, TOKEN_RIGHT_BRACKET, "Expect ']' after list elements.");
}

// A map literal.
static void map(CompilerBase* compiler, bool canAssign)
{
  Compiler* derived = compiler->derived;

  // Instantiate a new map.
  loadCoreVariable(derived, "Map");
  callMethod(derived, 0, "new()", 5);

  // Compile the map elements. Each one is compiled to just invoke the
  // subscript setter on the map.
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
    callMethod(derived, 2, "addCore_(_,_)", 13);
  } while (match(compiler->parser, TOKEN_COMMA));

  // Allow newlines before the closing '}'.
  ignoreNewlines(compiler->parser);
  consume(compiler->parser, TOKEN_RIGHT_BRACE, "Expect '}' after map entries.");
}

// Unary operators like `-foo`.
static void unaryOp(CompilerBase* compiler, bool canAssign)
{
  GrammarRule* rule = getRule(compiler->parser->previous.type);

  ignoreNewlines(compiler->parser);

  // Compile the argument.
  parsePrecedence(compiler, (Precedence)(PREC_UNARY + 1));

  // Call the operator method on the left-hand side.
  Compiler* derived = compiler->derived;
  callMethod(derived, 0, rule->name, 1);
}

static void boolean(CompilerBase* compiler, bool canAssign)
{
  Compiler* derived = compiler->derived;
  emitOp(derived,
      compiler->parser->previous.type == TOKEN_FALSE ? CODE_FALSE : CODE_TRUE);
}

// Walks the compiler chain to find the compiler for the nearest class
// enclosing this one. Returns NULL if not currently inside a class definition.
static Compiler* getEnclosingClassCompiler(Compiler* compiler)
{
  while (compiler != NULL)
  {
    if (compiler->enclosingClass != NULL) return compiler;
    compiler = GET_PARENT(compiler);
  }

  return NULL;
}

// Walks the compiler chain to find the nearest class enclosing this one.
// Returns NULL if not currently inside a class definition.
static ClassInfo* getEnclosingClass(Compiler* compiler)
{
  compiler = getEnclosingClassCompiler(compiler);
  return compiler == NULL ? NULL : compiler->enclosingClass;
}

static void field(CompilerBase* compiler, bool canAssign)
{
  // Initialize it with a fake value so we can keep parsing and minimize the
  // number of cascaded errors.
  int field = MAX_FIELDS;

  Compiler* derived = compiler->derived;
  ClassInfo* enclosingClass = getEnclosingClass(derived);

  if (enclosingClass == NULL)
  {
    error(compiler->parser, "Cannot reference a field outside of a class definition.");
  }
  else if (enclosingClass->isForeign)
  {
    error(compiler->parser, "Cannot define fields in a foreign class.");
  }
  else if (enclosingClass->inStatic)
  {
    error(compiler->parser, "Cannot use an instance field in a static method.");
  }
  else
  {
    // Look up the field, or implicitly define it.
    field = wrenSymbolTableEnsure(compiler->parser->vm, &enclosingClass->fields,
        compiler->parser->previous.start,
        compiler->parser->previous.length);

    if (field >= MAX_FIELDS)
    {
      error(compiler->parser, "A class can only have %d fields.", MAX_FIELDS);
    }
  }

  // If there's an "=" after a field name, it's an assignment.
  bool isLoad = true;
  if (canAssign && match(compiler->parser, TOKEN_EQ))
  {
    // Compile the right-hand side.
    expression(compiler);
    isLoad = false;
  }

  // If we're directly inside a method, use a more optimal instruction.
  Compiler* parent = GET_PARENT(derived);
  if (parent != NULL &&
      parent->enclosingClass == enclosingClass)
  {
    emitByteArg(derived, isLoad ? CODE_LOAD_FIELD_THIS : CODE_STORE_FIELD_THIS,
                field);
  }
  else
  {
    loadThis(derived);
    emitByteArg(derived, isLoad ? CODE_LOAD_FIELD : CODE_STORE_FIELD, field);
  }

  allowLineBeforeDot(compiler->parser);
}

// Compiles a read or assignment to [variable].
static void bareName(Compiler* compiler, bool canAssign, Variable variable)
{
  // If there's an "=" after a bare name, it's a variable assignment.
  if (canAssign && match(compiler->base.parser, TOKEN_EQ))
  {
    // Compile the right-hand side.
    expression(&compiler->base);

    // Emit the store instruction.
    switch (variable.scope)
    {
      case SCOPE_LOCAL:
        emitByteArg(compiler, CODE_STORE_LOCAL, variable.index);
        break;
      case SCOPE_UPVALUE:
        emitByteArg(compiler, CODE_STORE_UPVALUE, variable.index);
        break;
      case SCOPE_MODULE:
        emitShortArg(compiler, CODE_STORE_MODULE_VAR, variable.index);
        break;
      default:
        UNREACHABLE();
    }
    return;
  }

  // Emit the load instruction.
  loadVariable(compiler, variable);

  allowLineBeforeDot(compiler->base.parser);
}

static void staticField(CompilerBase* compiler, bool canAssign)
{
  Compiler* derived = compiler->derived;
  Compiler* classCompiler = getEnclosingClassCompiler(derived);
  if (classCompiler == NULL)
  {
    error(compiler->parser, "Cannot use a static field outside of a class definition.");
    return;
  }

  // Look up the name in the scope chain.
  Token* token = &compiler->parser->previous;

  // If this is the first time we've seen this static field, implicitly
  // define it as a variable in the scope surrounding the class definition.
  if (resolveLocal(&classCompiler->base, token->start, token->length) == -1)
  {
    int symbol = declareVariable(&classCompiler->base, NULL);

    // Implicitly initialize it to null.
    emitOp(classCompiler, CODE_NULL);
    defineVariable(classCompiler, symbol);
  }

  // It definitely exists now, so resolve it properly. This is different from
  // the above resolveLocal() call because we may have already closed over it
  // as an upvalue.
  Variable variable = resolveName(derived, token->start, token->length);
  bareName(derived, canAssign, variable);
}

// Compiles a variable name or method call with an implicit receiver.
static void name(CompilerBase* compiler, bool canAssign)
{
  Compiler* derived = compiler->derived;

  // Look for the name in the scope chain up to the nearest enclosing method.
  Token* token = &compiler->parser->previous;

  Variable variable = resolveNonmodule(derived, token->start, token->length);
  if (variable.index != -1)
  {
    bareName(derived, canAssign, variable);
    return;
  }

  // TODO: The fact that we return above here if the variable is known and parse
  // an optional argument list below if not means that the grammar is not
  // context-free. A line of code in a method like "someName(foo)" is a parse
  // error if "someName" is a defined variable in the surrounding scope and not
  // if it isn't. Fix this. One option is to have "someName(foo)" always
  // resolve to a self-call if there is an argument list, but that makes
  // getters a little confusing.

  // If we're inside a method and the name is lowercase, treat it as a method
  // on this.
  if (wrenIsLocalName(token->start) && getEnclosingClass(derived) != NULL)
  {
    loadThis(derived);
    namedCall(derived, canAssign, CODE_CALL_0);
    return;
  }

  // Otherwise, look for a module-level variable with the name.
  variable.scope = SCOPE_MODULE;
  variable.index = wrenSymbolTableFind(&derived->module->variableNames,
                                       token->start, token->length);
  if (variable.index == -1)
  {
    // Implicitly define a module-level variable in
    // the hopes that we get a real definition later.
    variable.index = wrenDeclareVariable(compiler->parser->vm,
                                         derived->module,
                                         token->start, token->length,
                                         token->line);

    if (variable.index == -2)
    {
      error(compiler->parser, "Too many module variables defined.");
    }
  }

  bareName(derived, canAssign, variable);
}

static void null(CompilerBase* compiler, bool canAssign)
{
  Compiler* derived = compiler->derived;
  emitOp(derived, CODE_NULL);
}

// A number or string literal.
static void literal(CompilerBase* compiler, bool canAssign)
{
  Compiler* derived = compiler->derived;
  emitConstant(derived, compiler->parser->previous.value);
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
  Compiler* derived = compiler->derived;

  // Instantiate a new list.
  loadCoreVariable(derived, "List");
  callMethod(derived, 0, "new()", 5);

  do
  {
    // The opening string part.
    literal(compiler, false);
    callMethod(derived, 1, "addCore_(_)", 11);

    // The interpolated expression.
    ignoreNewlines(compiler->parser);
    expression(compiler);
    callMethod(derived, 1, "addCore_(_)", 11);

    ignoreNewlines(compiler->parser);
  } while (match(compiler->parser, TOKEN_INTERPOLATION));

  // The trailing string part.
  consume(compiler->parser, TOKEN_STRING, "Expect end of string interpolation.");
  literal(compiler, false);
  callMethod(derived, 1, "addCore_(_)", 11);

  // The list of interpolated parts.
  callMethod(derived, 0, "join()", 6);
}

static void super_(CompilerBase* compiler, bool canAssign)
{
  Compiler* derived = compiler->derived;
  ClassInfo* enclosingClass = getEnclosingClass(derived);
  if (enclosingClass == NULL)
  {
    error(compiler->parser, "Cannot use 'super' outside of a method.");
  }

  loadThis(derived);

  // TODO: Super operator calls.
  // TODO: There's no syntax for invoking a superclass constructor with a
  // different name from the enclosing one. Figure that out.

  // See if it's a named super call, or an unnamed one.
  if (match(compiler->parser, TOKEN_DOT))
  {
    // Compile the superclass call.
    consume(compiler->parser, TOKEN_NAME, "Expect method name after 'super.'.");
    namedCall(derived, canAssign, CODE_SUPER_0);
  }
  else if (enclosingClass != NULL)
  {
    // No explicit name, so use the name of the enclosing method. Make sure we
    // check that enclosingClass isn't NULL first. We've already reported the
    // error, but we don't want to crash here.
    methodCall(derived, CODE_SUPER_0, enclosingClass->signature);
  }
}

static void this_(CompilerBase* compiler, bool canAssign)
{
  Compiler* derived = compiler->derived;
  if (getEnclosingClass(derived) == NULL)
  {
    error(compiler->parser, "Cannot use 'this' outside of a method.");
    return;
  }

  loadThis(derived);
}

// Subscript or "array indexing" operator like `foo[bar]`.
static void subscript(CompilerBase* compiler, bool canAssign)
{
  Signature signature = { "", 0, SIG_SUBSCRIPT, 0 };

  // Parse the argument list.
  finishArgumentList(compiler, &signature.arity);
  consume(compiler->parser, TOKEN_RIGHT_BRACKET, "Expect ']' after arguments.");

  allowLineBeforeDot(compiler->parser);

  if (canAssign && match(compiler->parser, TOKEN_EQ))
  {
    signature.type = SIG_SUBSCRIPT_SETTER;

    // Compile the assigned value.
    validateNumParameters(compiler->parser, ++signature.arity);
    expression(compiler);
  }

  Compiler* derived = compiler->derived;
  callSignature(derived, CODE_CALL_0, &signature);
}

static void call(CompilerBase* compiler, bool canAssign)
{
  ignoreNewlines(compiler->parser);
  consume(compiler->parser, TOKEN_NAME, "Expect method name after '.'.");

  Compiler* derived = compiler->derived;
  namedCall(derived, canAssign, CODE_CALL_0);
}

static void and_(CompilerBase* compiler, bool canAssign)
{
  ignoreNewlines(compiler->parser);

  // Skip the right argument if the left is false.
  Compiler* derived = compiler->derived;
  int jump = emitJump(derived, CODE_AND);
  parsePrecedence(compiler, PREC_LOGICAL_AND);
  patchJump(derived, jump);
}

static void or_(CompilerBase* compiler, bool canAssign)
{
  ignoreNewlines(compiler->parser);

  // Skip the right argument if the left is true.
  Compiler* derived = compiler->derived;
  int jump = emitJump(derived, CODE_OR);
  parsePrecedence(compiler, PREC_LOGICAL_OR);
  patchJump(derived, jump);
}

static void conditional(CompilerBase* compiler, bool canAssign)
{
  // Ignore newline after '?'.
  ignoreNewlines(compiler->parser);

  Compiler* derived = compiler->derived;

  // Jump to the else branch if the condition is false.
  int ifJump = emitJump(derived, CODE_JUMP_IF);

  // Compile the then branch.
  parsePrecedence(compiler, PREC_CONDITIONAL);

  consume(compiler->parser, TOKEN_COLON,
          "Expect ':' after then branch of conditional operator.");
  ignoreNewlines(compiler->parser);

  // Jump over the else branch when the if branch is taken.
  int elseJump = emitJump(derived, CODE_JUMP);

  // Compile the else branch.
  patchJump(derived, ifJump);

  parsePrecedence(compiler, PREC_ASSIGNMENT);

  // Patch the jump over the else.
  patchJump(derived, elseJump);
}

void infixOp(CompilerBase* compiler, bool canAssign)
{
  GrammarRule* rule = getRule(compiler->parser->previous.type);

  // An infix operator cannot end an expression.
  ignoreNewlines(compiler->parser);

  // Compile the right-hand side.
  parsePrecedence(compiler, (Precedence)(rule->precedence + 1));

  // Call the operator method on the left-hand side.
  Signature signature = { rule->name, (int)strlen(rule->name), SIG_METHOD, 1 };
  Compiler* derived = compiler->derived;
  callSignature(derived, CODE_CALL_0, &signature);
}

// Returns the number of bytes for the arguments to the instruction 
// at [ip] in [fn]'s bytecode.
static int getByteCountForArguments(const uint8_t* bytecode,
                            const Value* constants, int ip)
{
  Code instruction = (Code)bytecode[ip];
  switch (instruction)
  {
    case CODE_NULL:
    case CODE_FALSE:
    case CODE_TRUE:
    case CODE_POP:
    case CODE_CLOSE_UPVALUE:
    case CODE_RETURN:
    case CODE_END:
    case CODE_LOAD_LOCAL_0:
    case CODE_LOAD_LOCAL_1:
    case CODE_LOAD_LOCAL_2:
    case CODE_LOAD_LOCAL_3:
    case CODE_LOAD_LOCAL_4:
    case CODE_LOAD_LOCAL_5:
    case CODE_LOAD_LOCAL_6:
    case CODE_LOAD_LOCAL_7:
    case CODE_LOAD_LOCAL_8:
    case CODE_CONSTRUCT:
    case CODE_FOREIGN_CONSTRUCT:
    case CODE_FOREIGN_CLASS:
    case CODE_END_MODULE:
    case CODE_END_CLASS:
      return 0;

    case CODE_LOAD_LOCAL:
    case CODE_STORE_LOCAL:
    case CODE_LOAD_UPVALUE:
    case CODE_STORE_UPVALUE:
    case CODE_LOAD_FIELD_THIS:
    case CODE_STORE_FIELD_THIS:
    case CODE_LOAD_FIELD:
    case CODE_STORE_FIELD:
    case CODE_CLASS:
      return 1;

    case CODE_CONSTANT:
    case CODE_LOAD_MODULE_VAR:
    case CODE_STORE_MODULE_VAR:
    case CODE_CALL_0:
    case CODE_CALL_1:
    case CODE_CALL_2:
    case CODE_CALL_3:
    case CODE_CALL_4:
    case CODE_CALL_5:
    case CODE_CALL_6:
    case CODE_CALL_7:
    case CODE_CALL_8:
    case CODE_CALL_9:
    case CODE_CALL_10:
    case CODE_CALL_11:
    case CODE_CALL_12:
    case CODE_CALL_13:
    case CODE_CALL_14:
    case CODE_CALL_15:
    case CODE_CALL_16:
    case CODE_JUMP:
    case CODE_LOOP:
    case CODE_JUMP_IF:
    case CODE_AND:
    case CODE_OR:
    case CODE_METHOD_INSTANCE:
    case CODE_METHOD_STATIC:
    case CODE_IMPORT_MODULE:
    case CODE_IMPORT_VARIABLE:
      return 2;

    case CODE_SUPER_0:
    case CODE_SUPER_1:
    case CODE_SUPER_2:
    case CODE_SUPER_3:
    case CODE_SUPER_4:
    case CODE_SUPER_5:
    case CODE_SUPER_6:
    case CODE_SUPER_7:
    case CODE_SUPER_8:
    case CODE_SUPER_9:
    case CODE_SUPER_10:
    case CODE_SUPER_11:
    case CODE_SUPER_12:
    case CODE_SUPER_13:
    case CODE_SUPER_14:
    case CODE_SUPER_15:
    case CODE_SUPER_16:
      return 4;

    case CODE_CLOSURE:
    {
      int constant = (bytecode[ip + 1] << 8) | bytecode[ip + 2];
      ObjFn* loadedFn = AS_FN(constants[constant]);

      // There are two bytes for the constant, then two for each upvalue.
      return 2 + (loadedFn->numUpvalues * 2);
    }
  }

  UNREACHABLE();
  return 0;
}

// Marks the beginning of a loop. Keeps track of the current instruction so we
// know what to loop back to at the end of the body.
static void startLoop(Compiler* compiler, Loop* loop)
{
  loop->enclosing = compiler->loop;
  loop->start = compiler->fn->code.count - 1;
  loop->scopeDepth = compiler->base.scopeDepth;
  compiler->loop = loop;
}

// Emits the [CODE_JUMP_IF] instruction used to test the loop condition and
// potentially exit the loop. Keeps track of the instruction so we can patch it
// later once we know where the end of the body is.
static void testExitLoop(Compiler* compiler)
{
  compiler->loop->exitJump = emitJump(compiler, CODE_JUMP_IF);
}

// Compiles the body of the loop and tracks its extent so that contained "break"
// statements can be handled correctly.
static void loopBody(Compiler* compiler)
{
  compiler->loop->body = compiler->fn->code.count;
  statement(&compiler->base);
}

// Ends the current innermost loop. Patches up all jumps and breaks now that
// we know where the end of the loop is.
static void endLoop(Compiler* compiler)
{
  // We don't check for overflow here since the forward jump over the loop body
  // will report an error for the same problem.
  int loopOffset = compiler->fn->code.count - compiler->loop->start + 2;
  emitShortArg(compiler, CODE_LOOP, loopOffset);

  patchJump(compiler, compiler->loop->exitJump);

  // Find any break placeholder instructions (which will be CODE_END in the
  // bytecode) and replace them with real jumps.
  int i = compiler->loop->body;
  while (i < compiler->fn->code.count)
  {
    if (compiler->fn->code.data[i] == CODE_END)
    {
      compiler->fn->code.data[i] = CODE_JUMP;
      patchJump(compiler, i + 1);
      i += 3;
    }
    else
    {
      // Skip this instruction and its arguments.
      i += 1 + getByteCountForArguments(compiler->fn->code.data,
                               compiler->fn->constants.data, i);
    }
  }

  compiler->loop = compiler->loop->enclosing;
}

static void breakStatement(CompilerBase* compiler)
{
  Compiler* derived = compiler->derived;

  if (derived->loop == NULL)
  {
    error(derived->base.parser, "Cannot use 'break' outside of a loop.");
    return;
  }

  // Since we will be jumping out of the scope, make sure any locals in it
  // are discarded first.
  discardLocals(compiler, derived->loop->scopeDepth + 1);

  // Emit a placeholder instruction for the jump to the end of the body. When
  // we're done compiling the loop body and know where the end is, we'll
  // replace these with `CODE_JUMP` instructions with appropriate offsets.
  // We use `CODE_END` here because that can't occur in the middle of
  // bytecode.
  emitJump(derived, CODE_END);
}

static void continueStatement(CompilerBase* compiler)
{
  Compiler* derived = compiler->derived;

  if (derived->loop == NULL)
  {
    error(derived->base.parser, "Cannot use 'continue' outside of a loop.");
    return;
  }

  // Since we will be jumping out of the scope, make sure any locals in it
  // are discarded first.
  discardLocals(compiler, derived->loop->scopeDepth + 1);

  // emit a jump back to the top of the loop
  int loopOffset = derived->fn->code.count - derived->loop->start + 2;
  emitShortArg(derived, CODE_LOOP, loopOffset);
}

static void returnStatement(CompilerBase* compiler)
{
  Compiler* derived = compiler->derived;

  // Compile the return value.
  if (peek(derived->base.parser) == TOKEN_LINE)
  {
    // If there's no expression after return, initializers should 
    // return 'this' and regular methods should return null
    Code result = derived->isInitializer ? CODE_LOAD_LOCAL_0 : CODE_NULL;
    emitOp(derived, result);
  }
  else
  {
    if (derived->isInitializer)
    {
      error(derived->base.parser, "A constructor cannot return a value.");
    }

    expression(compiler);
  }

  emitOp(derived, CODE_RETURN);
}

static void forStatement(CompilerBase* compiler)
{
  // A for statement like:
  //
  //     for (i in sequence.expression) {
  //       System.print(i)
  //     }
  //
  // Is compiled to bytecode almost as if the source looked like this:
  //
  //     {
  //       var seq_ = sequence.expression
  //       var iter_
  //       while (iter_ = seq_.iterate(iter_)) {
  //         var i = seq_.iteratorValue(iter_)
  //         System.print(i)
  //       }
  //     }
  //
  // It's not exactly this, because the synthetic variables `seq_` and `iter_`
  // actually get names that aren't valid Wren identfiers, but that's the basic
  // idea.
  //
  // The important parts are:
  // - The sequence expression is only evaluated once.
  // - The .iterate() method is used to advance the iterator and determine if
  //   it should exit the loop.
  // - The .iteratorValue() method is used to get the value at the current
  //   iterator position.

  // Create a scope for the hidden local variables used for the iterator.
  pushScope(compiler);

  consume(compiler->parser, TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
  consume(compiler->parser, TOKEN_NAME, "Expect for loop variable name.");

  // Remember the name of the loop variable.
  const char* name = compiler->parser->previous.start;
  int length = compiler->parser->previous.length;

  consume(compiler->parser, TOKEN_IN, "Expect 'in' after loop variable.");
  ignoreNewlines(compiler->parser);

  // Evaluate the sequence expression and store it in a hidden local variable.
  // The space in the variable name ensures it won't collide with a user-defined
  // variable.
  expression(compiler);

  // Verify that there is space to hidden local variables.
  // Note that we expect only two addLocal calls next to each other in the
  // following code.
  if (compiler->numLocals + 2 > MAX_LOCALS)
  {
    error(compiler->parser, "Cannot declare more than %d variables in one scope. (Not enough space for for-loops internal variables)",
          MAX_LOCALS);
    return;
  }
  int seqSlot = addLocal(compiler, "seq ", 4);

  // Create another hidden local for the iterator object.
  null(compiler, false);
  int iterSlot = addLocal(compiler, "iter ", 5);

  consume(compiler->parser, TOKEN_RIGHT_PAREN, "Expect ')' after loop expression.");

  Compiler* derived = compiler->derived;

  Loop loop;
  startLoop(derived, &loop);

  // Advance the iterator by calling the ".iterate" method on the sequence.
  loadLocal(derived, seqSlot);
  loadLocal(derived, iterSlot);

  // Update and test the iterator.
  callMethod(derived, 1, "iterate(_)", 10);
  emitByteArg(derived, CODE_STORE_LOCAL, iterSlot);
  testExitLoop(derived);

  // Get the current value in the sequence by calling ".iteratorValue".
  loadLocal(derived, seqSlot);
  loadLocal(derived, iterSlot);
  callMethod(derived, 1, "iteratorValue(_)", 16);

  // Bind the loop variable in its own scope. This ensures we get a fresh
  // variable each iteration so that closures for it don't all see the same one.
  pushScope(compiler);
  addLocal(compiler, name, length);

  loopBody(derived);

  // Loop variable.
  popScope(compiler);

  endLoop(derived);

  // Hidden variables.
  popScope(compiler);
}

static void ifStatement(CompilerBase* compiler)
{
  // Compile the condition.
  consume(compiler->parser, TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
  expression(compiler);
  consume(compiler->parser, TOKEN_RIGHT_PAREN, "Expect ')' after if condition.");

  Compiler* derived = compiler->derived;

  // Jump to the else branch if the condition is false.
  int ifJump = emitJump(derived, CODE_JUMP_IF);

  // Compile the then branch.
  statement(compiler);

  // Compile the else branch if there is one.
  if (match(compiler->parser, TOKEN_ELSE))
  {
    // Jump over the else branch when the if branch is taken.
    int elseJump = emitJump(derived, CODE_JUMP);
    patchJump(derived, ifJump);

    statement(compiler);

    // Patch the jump over the else.
    patchJump(derived, elseJump);
  }
  else
  {
    patchJump(derived, ifJump);
  }
}

static void whileStatement(CompilerBase* compiler)
{
  Compiler* derived = compiler->derived;

  Loop loop;
  startLoop(derived, &loop);

  // Compile the condition.
  consume(compiler->parser, TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
  expression(compiler);
  consume(compiler->parser, TOKEN_RIGHT_PAREN, "Expect ')' after while condition.");

  testExitLoop(derived);
  loopBody(derived);
  endLoop(derived);
}

static void blockStatement(CompilerBase* compiler)
{
  Compiler* derived = compiler->derived;

  // Block statement.
  pushScope(compiler);
  if (finishBlock(compiler))
  {
    // Block was an expression, so discard it.
    emitOp(derived, CODE_POP);
  }
  popScope(compiler);
}

static void expressionStatement(CompilerBase* compiler)
{
  Compiler* derived = compiler->derived;

  // Expression statement.
  expression(compiler);
  emitOp(derived, CODE_POP);
}

// Creates a matching constructor method for an initializer with [signature]
// and [initializerSymbol].
//
// Construction is a two-stage process in Wren that involves two separate
// methods. There is a static method that allocates a new instance of the class.
// It then invokes an initializer method on the new instance, forwarding all of
// the constructor arguments to it.
//
// The allocator method always has a fixed implementation:
//
//     CODE_CONSTRUCT - Replace the class in slot 0 with a new instance of it.
//     CODE_CALL      - Invoke the initializer on the new instance.
//
// This creates that method and calls the initializer with [initializerSymbol].
static void createConstructor(Compiler* compiler, Signature* signature,
                              int initializerSymbol)
{
  Compiler methodCompiler;
  initCompiler(&methodCompiler, compiler->base.parser, compiler->module, compiler, true);

  // Allocate the instance.
  emitOp(&methodCompiler, compiler->enclosingClass->isForeign
       ? CODE_FOREIGN_CONSTRUCT : CODE_CONSTRUCT);

  // Run its initializer.
  emitShortArg(&methodCompiler, (Code)(CODE_CALL_0 + signature->arity),
               initializerSymbol);

  // Return the instance.
  emitOp(&methodCompiler, CODE_RETURN);

  endCompiler(&methodCompiler, "", 0);
}

// Loads the enclosing class onto the stack and then binds the function already
// on the stack as a method on that class.
static void defineMethod(Compiler* compiler, Variable classVariable,
                         bool isStatic, int methodSymbol)
{
  // Load the class. We have to do this for each method because we can't
  // keep the class on top of the stack. If there are static fields, they
  // will be locals above the initial variable slot for the class on the
  // stack. To skip past those, we just load the class each time right before
  // defining a method.
  loadVariable(compiler, classVariable);

  // Define the method.
  Code instruction = isStatic ? CODE_METHOD_STATIC : CODE_METHOD_INSTANCE;
  emitShortArg(compiler, instruction, methodSymbol);
}

// Declares a method in the enclosing class with [signature].
//
// Reports an error if a method with that signature is already declared.
// Returns the symbol for the method.
static int declareMethod(Compiler* compiler, Signature* signature,
                         const char* name, int length)
{
  int symbol = signatureSymbol(compiler, signature);

  // See if the class has already declared method with this signature.
  ClassInfo* classInfo = compiler->enclosingClass;
  IntBuffer* methods = classInfo->inStatic
      ? &classInfo->staticMethods : &classInfo->methods;
  for (int i = 0; i < methods->count; i++)
  {
    if (methods->data[i] == symbol)
    {
      const char* staticPrefix = classInfo->inStatic ? "static " : "";
      error(compiler->base.parser, "Class %s already defines a %smethod '%s'.",
            &compiler->enclosingClass->name->value, staticPrefix, name);
      break;
    }
  }

  wrenIntBufferWrite(compiler->base.parser->vm, methods, symbol);
  return symbol;
}

// Compiles a method definition inside a class body.
//
// Returns `true` if it compiled successfully, or `false` if the method couldn't
// be parsed.
static bool method(Compiler* compiler, Variable classVariable)
{
  // Parse any attributes before the method and store them
  while (matchAttribute(&compiler->base)) {};

  // TODO: What about foreign constructors?
  bool isForeign = match(compiler->base.parser, TOKEN_FOREIGN);
  bool isStatic = match(compiler->base.parser, TOKEN_STATIC);
  compiler->enclosingClass->inStatic = isStatic;

  SignatureFn signatureFn = getRule(compiler->base.parser->current.type)->method;
  nextToken(compiler->base.parser);

  if (signatureFn == NULL)
  {
    error(compiler->base.parser, "Expect method definition.");
    return false;
  }

  // Build the method signature.
  Signature signature = signatureFromToken(compiler->base.parser, SIG_GETTER);
  compiler->enclosingClass->signature = &signature;

  Compiler methodCompiler;
  initCompiler(&methodCompiler, compiler->base.parser, compiler->module, compiler, true);

  // Compile the method signature.
  signatureFn(&methodCompiler.base, &signature);

  methodCompiler.isInitializer = signature.type == SIG_INITIALIZER;

  if (isStatic && signature.type == SIG_INITIALIZER)
  {
    error(compiler->base.parser, "A constructor cannot be static.");
  }

  // Include the full signature in debug messages in stack traces.
  char fullSignature[MAX_METHOD_SIGNATURE];
  int length;
  signatureToString(&signature, fullSignature, &length);

  // Copy any attributes the compiler collected into the enclosing class 
  copyMethodAttributes(compiler, isForeign, isStatic, fullSignature, length);

  // Check for duplicate methods. Doesn't matter that it's already been
  // defined, error will discard bytecode anyway.
  // Check if the method table already contains this symbol
  int methodSymbol = declareMethod(compiler, &signature, fullSignature, length);

  if (isForeign)
  {
    // Define a constant for the signature.
    emitConstant(compiler, wrenNewStringLength(compiler->base.parser->vm,
                                               fullSignature, length));

    // We don't need the function we started compiling in the parameter list
    // any more.
    methodCompiler.base.parser->vm->compiler = GET_PARENT(compiler);
  }
  else
  {
    consume(compiler->base.parser, TOKEN_LEFT_BRACE, "Expect '{' to begin method body.");
    finishBody(&methodCompiler);
    endCompiler(&methodCompiler, fullSignature, length);
  }

  // Define the method. For a constructor, this defines the instance
  // initializer method.
  defineMethod(compiler, classVariable, isStatic, methodSymbol);

  if (signature.type == SIG_INITIALIZER)
  {
    // Also define a matching constructor method on the metaclass.
    signature.type = SIG_METHOD;
    int constructorSymbol = signatureSymbol(compiler, &signature);

    createConstructor(compiler, &signature, methodSymbol);
    defineMethod(compiler, classVariable, true, constructorSymbol);
  }

  return true;
}

// Compiles a class definition. Assumes the "class" token has already been
// consumed (along with a possibly preceding "foreign" token).
static void classDefinition(CompilerBase* compiler, bool isForeign)
{
  Compiler* derived = compiler->derived;

  // Create a variable to store the class in.
  Variable classVariable;
  classVariable.scope = derived->base.scopeDepth == -1 ? SCOPE_MODULE : SCOPE_LOCAL;
  classVariable.index = declareNamedVariable(compiler);

  // Create shared class name value
  Value classNameString = wrenNewStringLength(compiler->parser->vm,
      compiler->parser->previous.start, compiler->parser->previous.length);

  // Create class name string to track method duplicates
  ObjString* className = AS_STRING(classNameString);

  // Make a string constant for the name.
  emitConstant(derived, classNameString);

  // Load the superclass (if there is one).
  if (match(compiler->parser, TOKEN_IS))
  {
    parsePrecedence(compiler, PREC_CALL);
  }
  else
  {
    // Implicitly inherit from Object.
    loadCoreVariable(derived, "Object");
  }

  // Store a placeholder for the number of fields argument. We don't know the
  // count until we've compiled all the methods to see which fields are used.
  int numFieldsInstruction = -1;
  if (isForeign)
  {
    emitOp(derived, CODE_FOREIGN_CLASS);
  }
  else
  {
    numFieldsInstruction = emitByteArg(derived, CODE_CLASS, 255);
  }

  // Store it in its name.
  defineVariable(derived, classVariable.index);

  // Push a local variable scope. Static fields in a class body are hoisted out
  // into local variables declared in this scope. Methods that use them will
  // have upvalues referencing them.
  pushScope(compiler);

  ClassInfo classInfo;
  classInfo.isForeign = isForeign;
  classInfo.name = className;

  // Allocate attribute maps if necessary. 
  // A method will allocate the methods one if needed
  classInfo.classAttributes = derived->attributes->count > 0
        ? wrenNewMap(compiler->parser->vm)
        : NULL;
  classInfo.methodAttributes = NULL;
  // Copy any existing attributes into the class
  copyAttributes(derived, classInfo.classAttributes);

  // Set up a symbol table for the class's fields. We'll initially compile
  // them to slots starting at zero. When the method is bound to the class, the
  // bytecode will be adjusted by [wrenBindMethod] to take inherited fields
  // into account.
  wrenSymbolTableInit(&classInfo.fields);

  // Set up symbol buffers to track duplicate static and instance methods.
  wrenIntBufferInit(&classInfo.methods);
  wrenIntBufferInit(&classInfo.staticMethods);
  derived->enclosingClass = &classInfo;

  // Compile the method definitions.
  consume(compiler->parser, TOKEN_LEFT_BRACE, "Expect '{' after class declaration.");
  matchLine(compiler->parser);

  while (!match(compiler->parser, TOKEN_RIGHT_BRACE))
  {
    if (!method(derived, classVariable)) break;

    // Don't require a newline after the last definition.
    if (match(compiler->parser, TOKEN_RIGHT_BRACE)) break;

    consumeLine(compiler->parser, "Expect newline after definition in class.");
  }

  // If any attributes are present, 
  // instantiate a ClassAttributes instance for the class
  // and send it over to CODE_END_CLASS
  bool hasAttr = classInfo.classAttributes != NULL || 
                 classInfo.methodAttributes != NULL;
  if(hasAttr) {
    emitClassAttributes(derived, &classInfo);
    loadVariable(derived, classVariable);
    // At the moment, we don't have other uses for CODE_END_CLASS,
    // so we put it inside this condition. Later, we can always
    // emit it and use it as needed.
    emitOp(derived, CODE_END_CLASS);
  }

  // Update the class with the number of fields.
  if (!isForeign)
  {
    derived->fn->code.data[numFieldsInstruction] =
        (uint8_t)classInfo.fields.count;
  }

  // Clear symbol tables for tracking field and method names.
  wrenSymbolTableClear(compiler->parser->vm, &classInfo.fields);
  wrenIntBufferClear(compiler->parser->vm, &classInfo.methods);
  wrenIntBufferClear(compiler->parser->vm, &classInfo.staticMethods);
  derived->enclosingClass = NULL;
  popScope(compiler);
}

// Compiles an "import" statement.
//
// An import compiles to a series of instructions. Given:
//
//     import "foo" for Bar, Baz
//
// We compile a single IMPORT_MODULE "foo" instruction to load the module
// itself. When that finishes executing the imported module, it leaves the
// ObjModule in vm->lastModule. Then, for Bar and Baz, we:
//
// * Declare a variable in the current scope with that name.
// * Emit an IMPORT_VARIABLE instruction to load the variable's value from the
//   other module.
// * Compile the code to store that value in the variable in this scope.
static void import(CompilerBase* compiler)
{
  Compiler* derived = compiler->derived;
  ignoreNewlines(compiler->parser);
  consume(compiler->parser, TOKEN_STRING, "Expect a string after 'import'.");
  int moduleConstant = addConstant(derived, compiler->parser->previous.value);

  // Load the module.
  emitShortArg(derived, CODE_IMPORT_MODULE, moduleConstant);

  // Discard the unused result value from calling the module body's closure.
  emitOp(derived, CODE_POP);

  // The for clause is optional.
  if (!match(compiler->parser, TOKEN_FOR)) return;

  // Compile the comma-separated list of variables to import.
  do
  {
    ignoreNewlines(compiler->parser);

    consume(compiler->parser, TOKEN_NAME, "Expect variable name.");

    // We need to hold onto the source variable, 
    // in order to reference it in the import later
    Token sourceVariableToken = compiler->parser->previous;

    // Define a string constant for the original variable name.
    int sourceVariableConstant = addConstant(derived,
          wrenNewStringLength(compiler->parser->vm,
                        sourceVariableToken.start,
                        sourceVariableToken.length));

    // Store the symbol we care about for the variable
    int slot = -1;
    if(match(compiler->parser, TOKEN_AS))
    {
      //import "module" for Source as Dest
      //Use 'Dest' as the name by declaring a new variable for it.
      //This parses a name after the 'as' and defines it.
      slot = declareNamedVariable(compiler);
    }
    else
    {
      //import "module" for Source
      //Uses 'Source' as the name directly
      slot = declareVariable(compiler, &sourceVariableToken);
    }

    // Load the variable from the other module.
    emitShortArg(derived, CODE_IMPORT_VARIABLE, sourceVariableConstant);

    // Store the result in the variable here.
    defineVariable(derived, slot);
  } while (match(compiler->parser, TOKEN_COMMA));
}

// Compiles a "var" variable definition statement.
static void variableDefinition(CompilerBase* compiler)
{
  Compiler* derived = compiler->derived;

  // Grab its name, but don't declare it yet. A (local) variable shouldn't be
  // in scope in its own initializer.
  consume(compiler->parser, TOKEN_NAME, "Expect variable name.");
  Token nameToken = compiler->parser->previous;

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

  // Now put it in scope.
  int symbol = declareVariable(compiler, &nameToken);
  defineVariable(derived, symbol);
}

ObjFn* wrenCompile(WrenVM* vm, ObjModule* module, const char* source,
                   bool isExpression, bool printErrors)
{
  // Skip the UTF-8 BOM if there is one.
  if (strncmp(source, "\xEF\xBB\xBF", 3) == 0) source += 3;

  Parser parser;
  parser.vm = vm;
  parser.source = source;
  parser.moduleName = (module && module->name) ? module->name->value : "<unknown>";

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

  parser.printErrors = printErrors;
  parser.hasError = false;

  // Read the first token into next
  nextToken(&parser);
  // Copy next -> current
  nextToken(&parser);

  int numExistingVariables = module->variables.count;

  Compiler compiler;
  initCompiler(&compiler, &parser, module, NULL, false);
  ignoreNewlines(&parser);

  if (isExpression)
  {
    expression(&compiler.base);
    consume(&parser, TOKEN_EOF, "Expect end of expression.");
  }
  else
  {
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

    emitOp(&compiler, CODE_END_MODULE);
  }

  emitOp(&compiler, CODE_RETURN);

  // See if there are any implicitly declared module-level variables that never
  // got an explicit definition. They will have values that are numbers
  // indicating the line where the variable was first used.
  for (int i = numExistingVariables; i < module->variables.count; i++)
  {
    if (IS_NUM(module->variables.data[i]))
    {
      // Synthesize a token for the original use site.
      parser.previous.type = TOKEN_NAME;
      parser.previous.start = module->variableNames.data[i]->value;
      parser.previous.length = module->variableNames.data[i]->length;
      parser.previous.line = (int)AS_NUM(module->variables.data[i]);
      error(&parser, "Variable is used but not defined.");
    }
  }
  
  return endCompiler(&compiler, "(script)", 8);
}

void wrenBindMethodCode(ObjClass* classObj, ObjFn* fn)
{
  int ip = 0;
  for (;;)
  {
    Code instruction = (Code)fn->code.data[ip];
    switch (instruction)
    {
      case CODE_LOAD_FIELD:
      case CODE_STORE_FIELD:
      case CODE_LOAD_FIELD_THIS:
      case CODE_STORE_FIELD_THIS:
        // Shift this class's fields down past the inherited ones. We don't
        // check for overflow here because we'll see if the number of fields
        // overflows when the subclass is created.
        fn->code.data[ip + 1] += classObj->superclass->numFields;
        break;

      case CODE_SUPER_0:
      case CODE_SUPER_1:
      case CODE_SUPER_2:
      case CODE_SUPER_3:
      case CODE_SUPER_4:
      case CODE_SUPER_5:
      case CODE_SUPER_6:
      case CODE_SUPER_7:
      case CODE_SUPER_8:
      case CODE_SUPER_9:
      case CODE_SUPER_10:
      case CODE_SUPER_11:
      case CODE_SUPER_12:
      case CODE_SUPER_13:
      case CODE_SUPER_14:
      case CODE_SUPER_15:
      case CODE_SUPER_16:
      {
        // Fill in the constant slot with a reference to the superclass.
        int constant = (fn->code.data[ip + 3] << 8) | fn->code.data[ip + 4];
        fn->constants.data[constant] = OBJ_VAL(classObj->superclass);
        break;
      }

      case CODE_CLOSURE:
      {
        // Bind the nested closure too.
        int constant = (fn->code.data[ip + 1] << 8) | fn->code.data[ip + 2];
        wrenBindMethodCode(classObj, AS_FN(fn->constants.data[constant]));
        break;
      }

      case CODE_END:
        return;

      default:
        // Other instructions are unaffected, so just skip over them.
        break;
    }
    ip += 1 + getByteCountForArguments(fn->code.data, fn->constants.data, ip);
  }
}

void wrenMarkCompiler(WrenVM* vm, Compiler* compiler)
{
  wrenGrayValue(vm, compiler->base.parser->current.value);
  wrenGrayValue(vm, compiler->base.parser->previous.value);
  wrenGrayValue(vm, compiler->base.parser->next.value);

  // Walk up the parent chain to mark the outer compilers too. The VM only
  // tracks the innermost one.
  do
  {
    wrenGrayObj(vm, (Obj*)compiler->fn);
    wrenGrayObj(vm, (Obj*)compiler->constants);
    wrenGrayObj(vm, (Obj*)compiler->attributes);

    if (compiler->enclosingClass != NULL)
    {
      wrenBlackenSymbolTable(vm, &compiler->enclosingClass->fields);

      if(compiler->enclosingClass->methodAttributes != NULL) 
      {
        wrenGrayObj(vm, (Obj*)compiler->enclosingClass->methodAttributes);
      }
      if(compiler->enclosingClass->classAttributes != NULL) 
      {
        wrenGrayObj(vm, (Obj*)compiler->enclosingClass->classAttributes);
      }
    }

    compiler = GET_PARENT(compiler);
  }
  while (compiler != NULL);
}

// Helpers for Attributes

static void discardAttributes(CompilerBase* compiler)
{
  wrenMapClear(compiler->parser->vm, ((Compiler*)compiler->derived)->attributes);
}

// Add an attribute to a given group in the compiler attribues map
static void attributeDefinition(CompilerBase* compiler, bool runtimeAccess,
                                Value group, Value key, Value value) 
{
  if (!runtimeAccess)
  {
    // bytecode compiler only cares about runtime attributes
    return;
  }

  Compiler* derived = compiler->derived;

  WrenVM* vm = derived->base.parser->vm;

  if(IS_OBJ(group)) wrenPushRoot(vm, AS_OBJ(group));
  if(IS_OBJ(key))   wrenPushRoot(vm, AS_OBJ(key));
  if(IS_OBJ(value)) wrenPushRoot(vm, AS_OBJ(value));

  Value groupMapValue = wrenMapGet(derived->attributes, group);
  if(IS_UNDEFINED(groupMapValue)) 
  {
    groupMapValue = OBJ_VAL(wrenNewMap(vm));
    wrenMapSet(vm, derived->attributes, group, groupMapValue);
  }

  //we store them as a map per so we can maintain duplicate keys 
  //group = { key:[value, ...], }
  ObjMap* groupMap = AS_MAP(groupMapValue);

  //var keyItems = group[key]
  //if(!keyItems) keyItems = group[key] = [] 
  Value keyItemsValue = wrenMapGet(groupMap, key);
  if(IS_UNDEFINED(keyItemsValue)) 
  {
    keyItemsValue = OBJ_VAL(wrenNewList(vm, 0));
    wrenMapSet(vm, groupMap, key, keyItemsValue);
  }

  //keyItems.add(value)
  ObjList* keyItems = AS_LIST(keyItemsValue);
  wrenValueBufferWrite(vm, &keyItems->elements, value);

  if(IS_OBJ(group)) wrenPopRoot(vm);
  if(IS_OBJ(key))   wrenPopRoot(vm);
  if(IS_OBJ(value)) wrenPopRoot(vm);
}


// Emit the attributes in the give map onto the stack
static void emitAttributes(Compiler* compiler, ObjMap* attributes) 
{
  // Instantiate a new map for the attributes
  loadCoreVariable(compiler, "Map");
  callMethod(compiler, 0, "new()", 5);

  // The attributes are stored as group = { key:[value, value, ...] }
  // so our first level is the group map
  for(uint32_t groupIdx = 0; groupIdx < attributes->capacity; groupIdx++)
  {
    const MapEntry* groupEntry = &attributes->entries[groupIdx];
    if(IS_UNDEFINED(groupEntry->key)) continue;
    //group key
    emitConstant(compiler, groupEntry->key);

    //group value is gonna be a map
    loadCoreVariable(compiler, "Map");
    callMethod(compiler, 0, "new()", 5);

    ObjMap* groupItems = AS_MAP(groupEntry->value);
    for(uint32_t itemIdx = 0; itemIdx < groupItems->capacity; itemIdx++)
    {
      const MapEntry* itemEntry = &groupItems->entries[itemIdx];
      if(IS_UNDEFINED(itemEntry->key)) continue;

      emitConstant(compiler, itemEntry->key);
      // Attribute key value, key = []
      loadCoreVariable(compiler, "List");
      callMethod(compiler, 0, "new()", 5);
      // Add the items to the key list
      ObjList* items = AS_LIST(itemEntry->value);
      for(int elemIdx = 0; elemIdx < items->elements.count; ++elemIdx)
      {
        emitConstant(compiler, items->elements.data[elemIdx]);
        callMethod(compiler, 1, "addCore_(_)", 11);
      }
      // Add the list to the map
      callMethod(compiler, 2, "addCore_(_,_)", 13);
    }

    // Add the key/value to the map
    callMethod(compiler, 2, "addCore_(_,_)", 13);
  }

}

// Methods are stored as method <-> attributes, so we have to have 
// an indirection to resolve for methods
static void emitAttributeMethods(Compiler* compiler, ObjMap* attributes)
{
    // Instantiate a new map for the attributes
  loadCoreVariable(compiler, "Map");
  callMethod(compiler, 0, "new()", 5);

  for(uint32_t methodIdx = 0; methodIdx < attributes->capacity; methodIdx++)
  {
    const MapEntry* methodEntry = &attributes->entries[methodIdx];
    if(IS_UNDEFINED(methodEntry->key)) continue;
    emitConstant(compiler, methodEntry->key);
    ObjMap* attributeMap = AS_MAP(methodEntry->value);
    emitAttributes(compiler, attributeMap);
    callMethod(compiler, 2, "addCore_(_,_)", 13);
  }
}


// Emit the final ClassAttributes that exists at runtime
static void emitClassAttributes(Compiler* compiler, ClassInfo* classInfo)
{
  loadCoreVariable(compiler, "ClassAttributes");

  classInfo->classAttributes 
    ? emitAttributes(compiler, classInfo->classAttributes) 
    : null(&compiler->base, false);

  classInfo->methodAttributes 
    ? emitAttributeMethods(compiler, classInfo->methodAttributes) 
    : null(&compiler->base, false);

  callMethod(compiler, 2, "new(_,_)", 8);
}

// Copy the current attributes stored in the compiler into a destination map
static void copyAttributes(Compiler* compiler, ObjMap* into)
{
  if(compiler->attributes->count == 0) return;
  if(into == NULL) return;

  WrenVM* vm = compiler->base.parser->vm;
  
  // Note we copy the actual values as is since we'll take ownership 
  // and clear the original map
  for(uint32_t attrIdx = 0; attrIdx < compiler->attributes->capacity; attrIdx++)
  {
    const MapEntry* attrEntry = &compiler->attributes->entries[attrIdx];
    if(IS_UNDEFINED(attrEntry->key)) continue;
    wrenMapSet(vm, into, attrEntry->key, attrEntry->value);
  }
  
  wrenMapClear(vm, compiler->attributes);
}

// Copy the current attributes stored in the compiler into the method specific
// attributes for the current enclosingClass.
// This also resets the counter, since the intent is to consume the attributes
static void copyMethodAttributes(Compiler* compiler, bool isForeign,
            bool isStatic, const char* fullSignature, int32_t length) 
{
  if (compiler->attributes->count == 0) return;

  WrenVM* vm = compiler->base.parser->vm;
  
  // Make a map for this method to copy into
  ObjMap* methodAttr = wrenNewMap(vm);
  wrenPushRoot(vm, (Obj*)methodAttr);
  copyAttributes(compiler, methodAttr);

  // Include 'foreign static ' in front as needed
  int32_t fullLength = length;
  if (isForeign) fullLength += 8;
  if (isStatic) fullLength += 7;
  char fullSignatureWithPrefix[MAX_METHOD_SIGNATURE + 8 + 7];
  const char* foreignPrefix = isForeign ? "foreign " : "";
  const char* staticPrefix = isStatic ? "static " : "";
  sprintf(fullSignatureWithPrefix, "%s%s%.*s", foreignPrefix, staticPrefix, 
                                               length, fullSignature);
  fullSignatureWithPrefix[fullLength] = '\0';

  if (compiler->enclosingClass->methodAttributes == NULL)
  {
    compiler->enclosingClass->methodAttributes = wrenNewMap(vm);
  }

  // Store the method attributes in the class map
  Value key = wrenNewStringLength(vm, fullSignatureWithPrefix, fullLength);
  wrenMapSet(vm, compiler->enclosingClass->methodAttributes, key, OBJ_VAL(methodAttr));

  wrenPopRoot(vm);
}
