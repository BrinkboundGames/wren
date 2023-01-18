#include "wren_parser.h"
#include "wren_compiler_ops.h"

// The buffer size used to format a compile error message, excluding the header
// with the module name and error location. Using a hardcoded buffer for this
// is kind of hairy, but fortunately we can control what the longest possible
// message is and handle that. Ideally, we'd use `snprintf()`, but that's not
// available in standard C++98.
#define ERROR_MESSAGE_SIZE (80 + MAX_VARIABLE_NAME + 15)

static void printError(Parser* parser, int line, const char* label,
  const char* format, va_list args)
{
  parser->hasError = true;
  if (!parser->printErrors) return;

  // Only report errors if there is a WrenErrorFn to handle them.
  if (parser->vm->config.errorFn == NULL) return;

  // Format the label and message.
  char message[ERROR_MESSAGE_SIZE];
  int length = sprintf(message, "%s: ", label);
  length += vsprintf(message + length, format, args);
  ASSERT(length < ERROR_MESSAGE_SIZE, "Error should not exceed buffer.");

  parser->vm->config.errorFn(parser->vm, WREN_ERROR_COMPILE,
    parser->moduleName, line, message);
}

// Outputs a lexical error.
static void lexError(Parser* parser, const char* format, ...)
{
  va_list args;
  va_start(args, format);
  printError(parser, parser->currentLine, "Error", format, args);
  va_end(args);
}

// Outputs a compile or syntax error. This also marks the compilation as having
// an error, which ensures that the resulting code will be discarded and never
// run. This means that after calling error(), it's fine to generate whatever
// invalid bytecode you want since it won't be used.
//
// You'll note that most places that call error() continue to parse and compile
// after that. That's so that we can try to find as many compilation errors in
// one pass as possible instead of just bailing at the first one.
void error(Parser* parser, const char* format, ...)
{
  Token* token = &parser->previous;

  // If the parse error was caused by an error token, the lexer has already
  // reported it.
  if (token->type == TOKEN_ERROR) return;

  va_list args;
  va_start(args, format);
  if (token->type == TOKEN_LINE)
  {
    printError(parser, token->line, "Error at newline", format, args);
  }
  else if (token->type == TOKEN_EOF)
  {
    printError(parser, token->line,
      "Error at end of file", format, args);
  }
  else
  {
    // Make sure we don't exceed the buffer with a very long token.
    char label[10 + MAX_VARIABLE_NAME + 4 + 1];
    if (token->length <= MAX_VARIABLE_NAME)
    {
      sprintf(label, "Error at '%.*s'", token->length, token->start);
    }
    else
    {
      sprintf(label, "Error at '%.*s...'", MAX_VARIABLE_NAME, token->start);
    }
    printError(parser, token->line, label, format, args);
  }
  va_end(args);
}

// Lexing ----------------------------------------------------------------------

typedef struct
{
  const char* identifier;
  size_t      length;
  TokenType   tokenType;
} Keyword;

// The table of reserved words and their associated token types.
static Keyword keywords[] =
{
  {"break",     5, TOKEN_BREAK},
  {"continue",  8, TOKEN_CONTINUE},
  {"class",     5, TOKEN_CLASS},
  {"construct", 9, TOKEN_CONSTRUCT},
  {"else",      4, TOKEN_ELSE},
  {"false",     5, TOKEN_FALSE},
  {"for",       3, TOKEN_FOR},
  {"foreign",   7, TOKEN_FOREIGN},
  {"if",        2, TOKEN_IF},
  {"import",    6, TOKEN_IMPORT},
  {"as",        2, TOKEN_AS},
  {"in",        2, TOKEN_IN},
  {"is",        2, TOKEN_IS},
  {"null",      4, TOKEN_NULL},
  {"return",    6, TOKEN_RETURN},
  {"static",    6, TOKEN_STATIC},
  {"super",     5, TOKEN_SUPER},
  {"this",      4, TOKEN_THIS},
  {"true",      4, TOKEN_TRUE},
  {"var",       3, TOKEN_VAR},
  {"while",     5, TOKEN_WHILE},
  {NULL,        0, TOKEN_EOF} // Sentinel to mark the end of the array.
};

// Returns true if [c] is a valid (non-initial) identifier character.
static bool isName(char c)
{
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

// Returns true if [c] is a digit.
static bool isDigit(char c)
{
  return c >= '0' && c <= '9';
}

// Returns the current character the parser is sitting on.
static char peekChar(Parser* parser)
{
  return *parser->currentChar;
}

// Returns the character after the current character.
static char peekNextChar(Parser* parser)
{
  // If we're at the end of the source, don't read past it.
  if (peekChar(parser) == '\0') return '\0';
  return *(parser->currentChar + 1);
}

// Advances the parser forward one character.
static char nextChar(Parser* parser)
{
  char c = peekChar(parser);
  parser->currentChar++;
  if (c == '\n') parser->currentLine++;
  return c;
}

// If the current character is [c], consumes it and returns `true`.
static bool matchChar(Parser* parser, char c)
{
  if (peekChar(parser) != c) return false;
  nextChar(parser);
  return true;
}

// Sets the parser's current token to the given [type] and current character
// range.
static void makeToken(Parser* parser, TokenType type)
{
  parser->next.type = type;
  parser->next.start = parser->tokenStart;
  parser->next.length = (int)(parser->currentChar - parser->tokenStart);
  parser->next.line = parser->currentLine;

  // Make line tokens appear on the line containing the "\n".
  if (type == TOKEN_LINE) parser->next.line--;
}

// If the current character is [c], then consumes it and makes a token of type
// [two]. Otherwise makes a token of type [one].
static void twoCharToken(Parser* parser, char c, TokenType two, TokenType one)
{
  makeToken(parser, matchChar(parser, c) ? two : one);
}

// Skips the rest of the current line.
static void skipLineComment(Parser* parser)
{
  while (peekChar(parser) != '\n' && peekChar(parser) != '\0')
  {
    nextChar(parser);
  }
}

// Skips the rest of a block comment.
static void skipBlockComment(Parser* parser)
{
  int nesting = 1;
  while (nesting > 0)
  {
    if (peekChar(parser) == '\0')
    {
      lexError(parser, "Unterminated block comment.");
      return;
    }

    if (peekChar(parser) == '/' && peekNextChar(parser) == '*')
    {
      nextChar(parser);
      nextChar(parser);
      nesting++;
      continue;
    }

    if (peekChar(parser) == '*' && peekNextChar(parser) == '/')
    {
      nextChar(parser);
      nextChar(parser);
      nesting--;
      continue;
    }

    // Regular comment character.
    nextChar(parser);
  }
}

// Reads the next character, which should be a hex digit (0-9, a-f, or A-F) and
// returns its numeric value. If the character isn't a hex digit, returns -1.
static int readHexDigit(Parser* parser)
{
  char c = nextChar(parser);
  if (c >= '0' && c <= '9') return c - '0';
  if (c >= 'a' && c <= 'f') return c - 'a' + 10;
  if (c >= 'A' && c <= 'F') return c - 'A' + 10;

  // Don't consume it if it isn't expected. Keeps us from reading past the end
  // of an unterminated string.
  parser->currentChar--;
  return -1;
}

// Parses the numeric value of the current token.
static void makeNumber(Parser* parser, bool isHex)
{
  errno = 0;

  if (isHex)
  {
    parser->next.value = NUM_VAL((double)strtoll(parser->tokenStart, NULL, 16));
  }
  else
  {
    parser->next.value = NUM_VAL(strtod(parser->tokenStart, NULL));
  }

  if (errno == ERANGE)
  {
    lexError(parser, "Number literal was too large (%d).", sizeof(long int));
    parser->next.value = NUM_VAL(0);
  }

  // We don't check that the entire token is consumed after calling strtoll()
  // or strtod() because we've already scanned it ourselves and know it's valid.

  makeToken(parser, TOKEN_NUMBER);
}

// Finishes lexing a hexadecimal number literal.
static void readHexNumber(Parser* parser)
{
  // Skip past the `x` used to denote a hexadecimal literal.
  nextChar(parser);

  // Iterate over all the valid hexadecimal digits found.
  while (readHexDigit(parser) != -1) continue;

  makeNumber(parser, true);
}

// Finishes lexing a number literal.
static void readNumber(Parser* parser)
{
  while (isDigit(peekChar(parser))) nextChar(parser);

  // See if it has a floating point. Make sure there is a digit after the "."
  // so we don't get confused by method calls on number literals.
  if (peekChar(parser) == '.' && isDigit(peekNextChar(parser)))
  {
    nextChar(parser);
    while (isDigit(peekChar(parser))) nextChar(parser);
  }

  // See if the number is in scientific notation.
  if (matchChar(parser, 'e') || matchChar(parser, 'E'))
  {
    // Allow a single positive/negative exponent symbol.
    if (!matchChar(parser, '+'))
    {
      matchChar(parser, '-');
    }

    if (!isDigit(peekChar(parser)))
    {
      lexError(parser, "Unterminated scientific notation.");
    }

    while (isDigit(peekChar(parser))) nextChar(parser);
  }

  makeNumber(parser, false);
}

// Finishes lexing an identifier. Handles reserved words.
static void readName(Parser* parser, TokenType type, char firstChar)
{
  ByteBuffer string;
  wrenByteBufferInit(&string);
  wrenByteBufferWrite(parser->vm, &string, firstChar);

  while (isName(peekChar(parser)) || isDigit(peekChar(parser)))
  {
    char c = nextChar(parser);
    wrenByteBufferWrite(parser->vm, &string, c);
  }

  // Update the type if it's a keyword.
  size_t length = parser->currentChar - parser->tokenStart;
  for (int i = 0; keywords[i].identifier != NULL; i++)
  {
    if (length == keywords[i].length &&
      memcmp(parser->tokenStart, keywords[i].identifier, length) == 0)
    {
      type = keywords[i].tokenType;
      break;
    }
  }

  parser->next.value = wrenNewStringLength(parser->vm,
    (char*)string.data, string.count);

  wrenByteBufferClear(parser->vm, &string);
  makeToken(parser, type);
}

// Reads [digits] hex digits in a string literal and returns their number value.
static int readHexEscape(Parser* parser, int digits, const char* description)
{
  int value = 0;
  for (int i = 0; i < digits; i++)
  {
    if (peekChar(parser) == '"' || peekChar(parser) == '\0')
    {
      lexError(parser, "Incomplete %s escape sequence.", description);

      // Don't consume it if it isn't expected. Keeps us from reading past the
      // end of an unterminated string.
      parser->currentChar--;
      break;
    }

    int digit = readHexDigit(parser);
    if (digit == -1)
    {
      lexError(parser, "Invalid %s escape sequence.", description);
      break;
    }

    value = (value * 16) | digit;
  }

  return value;
}

// Reads a hex digit Unicode escape sequence in a string literal.
static void readUnicodeEscape(Parser* parser, ByteBuffer* string, int length)
{
  int value = readHexEscape(parser, length, "Unicode");

  // Grow the buffer enough for the encoded result.
  int numBytes = wrenUtf8EncodeNumBytes(value);
  if (numBytes != 0)
  {
    wrenByteBufferFill(parser->vm, string, 0, numBytes);
    wrenUtf8Encode(value, string->data + string->count - numBytes);
  }
}

static void readRawString(Parser* parser)
{
  ByteBuffer string;
  wrenByteBufferInit(&string);
  TokenType type = TOKEN_STRING;

  //consume the second and third "
  nextChar(parser);
  nextChar(parser);

  int skipStart = 0;
  int firstNewline = -1;

  int skipEnd = -1;
  int lastNewline = -1;

  for (;;)
  {
    char c = nextChar(parser);
    char c1 = peekChar(parser);
    char c2 = peekNextChar(parser);

    if (c == '\r') continue;

    if (c == '\n') {
      lastNewline = string.count;
      skipEnd = lastNewline;
      firstNewline = firstNewline == -1 ? string.count : firstNewline;
    }

    if (c == '"' && c1 == '"' && c2 == '"') break;

    bool isWhitespace = c == ' ' || c == '\t';
    skipEnd = c == '\n' || isWhitespace ? skipEnd : -1;

    // If we haven't seen a newline or other character yet, 
    // and still seeing whitespace, count the characters 
    // as skippable till we know otherwise
    bool skippable = skipStart != -1 && isWhitespace && firstNewline == -1;
    skipStart = skippable ? string.count + 1 : skipStart;

    // We've counted leading whitespace till we hit something else, 
    // but it's not a newline, so we reset skipStart since we need these characters
    if (firstNewline == -1 && !isWhitespace && c != '\n') skipStart = -1;

    if (c == '\0' || c1 == '\0' || c2 == '\0')
    {
      lexError(parser, "Unterminated raw string.");

      // Don't consume it if it isn't expected. Keeps us from reading past the
      // end of an unterminated string.
      parser->currentChar--;
      break;
    }

    wrenByteBufferWrite(parser->vm, &string, c);
  }

  //consume the second and third "
  nextChar(parser);
  nextChar(parser);

  int offset = 0;
  int count = string.count;

  if (firstNewline != -1 && skipStart == firstNewline) offset = firstNewline + 1;
  if (lastNewline != -1 && skipEnd == lastNewline) count = lastNewline;

  count -= (offset > count) ? count : offset;

  parser->next.value = wrenNewStringLength(parser->vm,
    ((char*)string.data) + offset, count);

  wrenByteBufferClear(parser->vm, &string);
  makeToken(parser, type);
}

// Finishes lexing a string literal.
static void readString(Parser* parser)
{
  ByteBuffer string;
  TokenType type = TOKEN_STRING;
  wrenByteBufferInit(&string);

  for (;;)
  {
    char c = nextChar(parser);
    if (c == '"') break;
    if (c == '\r') continue;

    if (c == '\0')
    {
      lexError(parser, "Unterminated string.");

      // Don't consume it if it isn't expected. Keeps us from reading past the
      // end of an unterminated string.
      parser->currentChar--;
      break;
    }

    if (c == '%')
    {
      if (parser->numParens < MAX_INTERPOLATION_NESTING)
      {
        // TODO: Allow format string.
        if (nextChar(parser) != '(') lexError(parser, "Expect '(' after '%%'.");

        parser->parens[parser->numParens++] = 1;
        type = TOKEN_INTERPOLATION;
        break;
      }

      lexError(parser, "Interpolation may only nest %d levels deep.",
        MAX_INTERPOLATION_NESTING);
    }

    if (c == '\\')
    {
      switch (nextChar(parser))
      {
      case '"':  wrenByteBufferWrite(parser->vm, &string, '"'); break;
      case '\\': wrenByteBufferWrite(parser->vm, &string, '\\'); break;
      case '%':  wrenByteBufferWrite(parser->vm, &string, '%'); break;
      case '0':  wrenByteBufferWrite(parser->vm, &string, '\0'); break;
      case 'a':  wrenByteBufferWrite(parser->vm, &string, '\a'); break;
      case 'b':  wrenByteBufferWrite(parser->vm, &string, '\b'); break;
      case 'e':  wrenByteBufferWrite(parser->vm, &string, '\33'); break;
      case 'f':  wrenByteBufferWrite(parser->vm, &string, '\f'); break;
      case 'n':  wrenByteBufferWrite(parser->vm, &string, '\n'); break;
      case 'r':  wrenByteBufferWrite(parser->vm, &string, '\r'); break;
      case 't':  wrenByteBufferWrite(parser->vm, &string, '\t'); break;
      case 'u':  readUnicodeEscape(parser, &string, 4); break;
      case 'U':  readUnicodeEscape(parser, &string, 8); break;
      case 'v':  wrenByteBufferWrite(parser->vm, &string, '\v'); break;
      case 'x':
        wrenByteBufferWrite(parser->vm, &string,
          (uint8_t)readHexEscape(parser, 2, "byte"));
        break;

      default:
        lexError(parser, "Invalid escape character '%c'.",
          *(parser->currentChar - 1));
        break;
      }
    }
    else
    {
      wrenByteBufferWrite(parser->vm, &string, c);
    }
  }

  parser->next.value = wrenNewStringLength(parser->vm,
    (char*)string.data, string.count);

  wrenByteBufferClear(parser->vm, &string);
  makeToken(parser, type);
}

// Lex the next token and store it in [parser.next].
void nextToken(Parser* parser)
{
  parser->previous = parser->current;
  parser->current = parser->next;

  // If we are out of tokens, don't try to tokenize any more. We *do* still
  // copy the TOKEN_EOF to previous so that code that expects it to be consumed
  // will still work.
  if (parser->next.type == TOKEN_EOF) return;
  if (parser->current.type == TOKEN_EOF) return;

  while (peekChar(parser) != '\0')
  {
    parser->tokenStart = parser->currentChar;

    char c = nextChar(parser);
    switch (c)
    {
    case '(':
      // If we are inside an interpolated expression, count the unmatched "(".
      if (parser->numParens > 0) parser->parens[parser->numParens - 1]++;
      makeToken(parser, TOKEN_LEFT_PAREN);
      return;

    case ')':
      // If we are inside an interpolated expression, count the ")".
      if (parser->numParens > 0 &&
        --parser->parens[parser->numParens - 1] == 0)
      {
        // This is the final ")", so the interpolation expression has ended.
        // This ")" now begins the next section of the template string.
        parser->numParens--;
        readString(parser);
        return;
      }

      makeToken(parser, TOKEN_RIGHT_PAREN);
      return;

    case '[': makeToken(parser, TOKEN_LEFT_BRACKET); return;
    case ']': makeToken(parser, TOKEN_RIGHT_BRACKET); return;
    case '{': makeToken(parser, TOKEN_LEFT_BRACE); return;
    case '}': makeToken(parser, TOKEN_RIGHT_BRACE); return;
    case ':': makeToken(parser, TOKEN_COLON); return;
    case ',': makeToken(parser, TOKEN_COMMA); return;
    case '*': makeToken(parser, TOKEN_STAR); return;
    case '%': makeToken(parser, TOKEN_PERCENT); return;
    case '#': {
      // Ignore shebang on the first line.
      if (parser->currentLine == 1 && peekChar(parser) == '!' && peekNextChar(parser) == '/')
      {
        skipLineComment(parser);
        break;
      }
      // Otherwise we treat it as a token
      makeToken(parser, TOKEN_HASH);
      return;
    }
    case '^': makeToken(parser, TOKEN_CARET); return;
    case '+': makeToken(parser, TOKEN_PLUS); return;
    case '-': makeToken(parser, TOKEN_MINUS); return;
    case '~': makeToken(parser, TOKEN_TILDE); return;
    case '?': makeToken(parser, TOKEN_QUESTION); return;

    case '|': twoCharToken(parser, '|', TOKEN_PIPEPIPE, TOKEN_PIPE); return;
    case '&': twoCharToken(parser, '&', TOKEN_AMPAMP, TOKEN_AMP); return;
    case '=': twoCharToken(parser, '=', TOKEN_EQEQ, TOKEN_EQ); return;
    case '!': twoCharToken(parser, '=', TOKEN_BANGEQ, TOKEN_BANG); return;

    case '.':
      if (matchChar(parser, '.'))
      {
        twoCharToken(parser, '.', TOKEN_DOTDOTDOT, TOKEN_DOTDOT);
        return;
      }

      makeToken(parser, TOKEN_DOT);
      return;

    case '/':
      if (matchChar(parser, '/'))
      {
        skipLineComment(parser);
        break;
      }

      if (matchChar(parser, '*'))
      {
        skipBlockComment(parser);
        break;
      }

      makeToken(parser, TOKEN_SLASH);
      return;

    case '<':
      if (matchChar(parser, '<'))
      {
        makeToken(parser, TOKEN_LTLT);
      }
      else
      {
        twoCharToken(parser, '=', TOKEN_LTEQ, TOKEN_LT);
      }
      return;

    case '>':
      if (matchChar(parser, '>'))
      {
        makeToken(parser, TOKEN_GTGT);
      }
      else
      {
        twoCharToken(parser, '=', TOKEN_GTEQ, TOKEN_GT);
      }
      return;

    case '\n':
      makeToken(parser, TOKEN_LINE);
      return;

    case ' ':
    case '\r':
    case '\t':
      // Skip forward until we run out of whitespace.
      while (peekChar(parser) == ' ' ||
        peekChar(parser) == '\r' ||
        peekChar(parser) == '\t')
      {
        nextChar(parser);
      }
      break;

    case '"': {
      if (peekChar(parser) == '"' && peekNextChar(parser) == '"') {
        readRawString(parser);
        return;
      }
      readString(parser); return;
    }
    case '_':
      readName(parser,
        peekChar(parser) == '_' ? TOKEN_STATIC_FIELD : TOKEN_FIELD, c);
      return;

    case '0':
      if (peekChar(parser) == 'x')
      {
        readHexNumber(parser);
        return;
      }

      readNumber(parser);
      return;

    default:
      if (isName(c))
      {
        readName(parser, TOKEN_NAME, c);
      }
      else if (isDigit(c))
      {
        readNumber(parser);
      }
      else
      {
        if (c >= 32 && c <= 126)
        {
          lexError(parser, "Invalid character '%c'.", c);
        }
        else
        {
          // Don't show non-ASCII values since we didn't UTF-8 decode the
          // bytes. Since there are no non-ASCII byte values that are
          // meaningful code units in Wren, the lexer works on raw bytes,
          // even though the source code and console output are UTF-8.
          lexError(parser, "Invalid byte 0x%x.", (uint8_t)c);
        }
        parser->next.type = TOKEN_ERROR;
        parser->next.length = 0;
      }
      return;
    }
  }

  // If we get here, we're out of source, so just make EOF tokens.
  parser->tokenStart = parser->currentChar;
  makeToken(parser, TOKEN_EOF);
}

TokenType peek(Parser* parser)
{
  return parser->current.type;
}

// Returns the type of the current token.
static TokenType peekNext(Parser* parser)
{
  return parser->next.type;
}

bool match(Parser* parser, TokenType expected)
{
  if (peek(parser) != expected) return false;

  nextToken(parser);
  return true;
}

void consume(Parser* parser, TokenType expected,
  const char* errorMessage)
{
  nextToken(parser);
  if (parser->previous.type != expected)
  {
    error(parser, errorMessage);

    // If the next token is the one we want, assume the current one is just a
    // spurious error and discard it to minimize the number of cascaded errors.
    if (parser->current.type == expected) nextToken(parser);
  }
}

bool matchLine(Parser* parser)
{
  if (!match(parser, TOKEN_LINE)) return false;

  while (match(parser, TOKEN_LINE));
  return true;
}

void ignoreNewlines(Parser* parser)
{
  matchLine(parser);
}

void consumeLine(Parser* parser, const char* errorMessage)
{
  consume(parser, TOKEN_LINE, errorMessage);
  ignoreNewlines(parser);
}

void allowLineBeforeDot(Parser* parser)
{
  if (peek(parser) == TOKEN_LINE && peekNext(parser) == TOKEN_DOT)
  {
    nextToken(parser);
  }
}

static Value consumeLiteral(Parser* parser, const char* message)
{
  if (match(parser, TOKEN_FALSE))  return FALSE_VAL;
  if (match(parser, TOKEN_TRUE))   return TRUE_VAL;
  if (match(parser, TOKEN_NUMBER)) return parser->previous.value;
  if (match(parser, TOKEN_STRING)) return parser->previous.value;
  if (match(parser, TOKEN_NAME))   return parser->previous.value;

  error(parser, message);
  nextToken(parser);
  return NULL_VAL;
}

// Compiles an optional setter parameter in a method [signature].
//
// Returns `true` if it was a setter.
static bool maybeSetter(CompilerBase* compiler, Signature* signature)
{
  // See if it's a setter.
  if (!match(compiler->parser, TOKEN_EQ)) return false;

  // It's a setter.
  if (signature->type == SIG_SUBSCRIPT)
  {
    signature->type = SIG_SUBSCRIPT_SETTER;
  }
  else
  {
    signature->type = SIG_SETTER;
  }

  // Parse the value parameter.
  consume(compiler->parser, TOKEN_LEFT_PAREN, "Expect '(' after '='.");
  declareNamedVariable(compiler);
  consume(compiler->parser, TOKEN_RIGHT_PAREN, "Expect ')' after parameter name.");

  signature->arity++;

  return true;
}

// Parses an optional parenthesized parameter list. Updates `type` and `arity`
// in [signature] to match what was parsed.
static void parameterList(CompilerBase* compiler, Signature* signature)
{
  // The parameter list is optional.
  if (!match(compiler->parser, TOKEN_LEFT_PAREN)) return;

  signature->type = SIG_METHOD;

  // Allow new line before an empty argument list
  ignoreNewlines(compiler->parser);

  // Allow an empty parameter list.
  if (match(compiler->parser, TOKEN_RIGHT_PAREN)) return;

  finishParameterList(compiler, &signature->arity);
  consume(compiler->parser, TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
}

// A parenthesized expression.
static void grouping(CompilerBase* compiler, bool canAssign)
{
  expression(compiler);
  consume(compiler->parser, TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

// Compiles a method signature for a constructor.
static void constructorSignature(CompilerBase* compiler, Signature* signature)
{
  consume(compiler->parser, TOKEN_NAME, "Expect constructor name after 'construct'.");

  // Capture the name.
  *signature = signatureFromToken(compiler->parser, SIG_INITIALIZER);

  if (match(compiler->parser, TOKEN_EQ))
  {
    error(compiler->parser, "A constructor cannot be a setter.");
  }

  if (!match(compiler->parser, TOKEN_LEFT_PAREN))
  {
    error(compiler->parser, "A constructor cannot be a getter.");
    return;
  }

  // Allow an empty parameter list.
  if (match(compiler->parser, TOKEN_RIGHT_PAREN)) return;

  finishParameterList(compiler, &signature->arity);
  consume(compiler->parser, TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
}

// Compiles a method signature for an infix operator.
static void infixSignature(CompilerBase* compiler, Signature* signature)
{
  // Add the RHS parameter.
  signature->type = SIG_METHOD;
  signature->arity = 1;

  // Parse the parameter name.
  consume(compiler->parser, TOKEN_LEFT_PAREN, "Expect '(' after operator name.");
  declareNamedVariable(compiler);
  consume(compiler->parser, TOKEN_RIGHT_PAREN, "Expect ')' after parameter name.");
}

// Compiles a method signature for an operator that can either be unary or
// infix (i.e. "-").
static void mixedSignature(CompilerBase* compiler, Signature* signature)
{
  signature->type = SIG_GETTER;

  // If there is a parameter, it's an infix operator, otherwise it's unary.
  if (match(compiler->parser, TOKEN_LEFT_PAREN))
  {
    // Add the RHS parameter.
    signature->type = SIG_METHOD;
    signature->arity = 1;

    // Parse the parameter name.
    declareNamedVariable(compiler);
    consume(compiler->parser, TOKEN_RIGHT_PAREN, "Expect ')' after parameter name.");
  }
}

// Compiles a method signature for a subscript operator.
static void subscriptSignature(CompilerBase* compiler, Signature* signature)
{
  signature->type = SIG_SUBSCRIPT;

  // The signature currently has "[" as its name since that was the token that
  // matched it. Clear that out.
  signature->length = 0;

  // Parse the parameters inside the subscript.
  finishParameterList(compiler, &signature->arity);
  consume(compiler->parser, TOKEN_RIGHT_BRACKET, "Expect ']' after parameters.");

  maybeSetter(compiler, signature);
}

// Compiles a method signature for a named method or setter.
static void namedSignature(CompilerBase* compiler, Signature* signature)
{
  signature->type = SIG_GETTER;

  // If it's a setter, it can't also have a parameter list.
  if (maybeSetter(compiler, signature)) return;

  // Regular named method with an optional parameter list.
  parameterList(compiler, signature);
}

// Compiles a method signature for an unary operator (i.e. "!").
static void unarySignature(CompilerBase* compiler, Signature* signature)
{
  // Do nothing. The name is already complete.
  signature->type = SIG_GETTER;
}

#define OP_CALL(term) static void term(CompilerBase* compiler, bool canAssign)\
            {\
            compiler->ops->term(compiler, canAssign);\
            }\

OP_CALL(list)
OP_CALL(subscript)
OP_CALL(map)
OP_CALL(call)
OP_CALL(infixOp)
OP_CALL(unaryOp)
OP_CALL(or_)
OP_CALL(and_)
OP_CALL(conditional)
OP_CALL(boolean)
OP_CALL(null)
OP_CALL(super_)
OP_CALL(this_)
OP_CALL(field)
OP_CALL(staticField)
OP_CALL(name)
OP_CALL(literal)
OP_CALL(stringInterpolation)

// This table defines all of the parsing rules for the prefix and infix
// expressions in the grammar. Expressions are parsed using a Pratt parser.
//
// See: http://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/
#define UNUSED                     { NULL, NULL, NULL, PREC_NONE, NULL }
#define PREFIX(fn)                 { fn, NULL, NULL, PREC_NONE, NULL }
#define INFIX(prec, fn)            { NULL, fn, NULL, prec, NULL }
#define INFIX_OPERATOR(prec, name) { NULL, infixOp, infixSignature, prec, name }
#define PREFIX_OPERATOR(name)      { unaryOp, NULL, unarySignature, PREC_NONE, name }
#define OPERATOR(name)             { unaryOp, infixOp, mixedSignature, PREC_TERM, name }

GrammarRule rules[] =
{
  /* TOKEN_LEFT_PAREN    */ PREFIX(grouping),
  /* TOKEN_RIGHT_PAREN   */ UNUSED,
  /* TOKEN_LEFT_BRACKET  */ { list, subscript, subscriptSignature, PREC_CALL, NULL },
  /* TOKEN_RIGHT_BRACKET */ UNUSED,
  /* TOKEN_LEFT_BRACE    */ PREFIX(map),
  /* TOKEN_RIGHT_BRACE   */ UNUSED,
  /* TOKEN_COLON         */ UNUSED,
  /* TOKEN_DOT           */ INFIX(PREC_CALL, call),
  /* TOKEN_DOTDOT        */ INFIX_OPERATOR(PREC_RANGE, ".."),
  /* TOKEN_DOTDOTDOT     */ INFIX_OPERATOR(PREC_RANGE, "..."),
  /* TOKEN_COMMA         */ UNUSED,
  /* TOKEN_STAR          */ INFIX_OPERATOR(PREC_FACTOR, "*"),
  /* TOKEN_SLASH         */ INFIX_OPERATOR(PREC_FACTOR, "/"),
  /* TOKEN_PERCENT       */ INFIX_OPERATOR(PREC_FACTOR, "%"),
  /* TOKEN_HASH          */ UNUSED,
  /* TOKEN_PLUS          */ INFIX_OPERATOR(PREC_TERM, "+"),
  /* TOKEN_MINUS         */ OPERATOR("-"),
  /* TOKEN_LTLT          */ INFIX_OPERATOR(PREC_BITWISE_SHIFT, "<<"),
  /* TOKEN_GTGT          */ INFIX_OPERATOR(PREC_BITWISE_SHIFT, ">>"),
  /* TOKEN_PIPE          */ INFIX_OPERATOR(PREC_BITWISE_OR, "|"),
  /* TOKEN_PIPEPIPE      */ INFIX(PREC_LOGICAL_OR, or_),
  /* TOKEN_CARET         */ INFIX_OPERATOR(PREC_BITWISE_XOR, "^"),
  /* TOKEN_AMP           */ INFIX_OPERATOR(PREC_BITWISE_AND, "&"),
  /* TOKEN_AMPAMP        */ INFIX(PREC_LOGICAL_AND, and_),
  /* TOKEN_BANG          */ PREFIX_OPERATOR("!"),
  /* TOKEN_TILDE         */ PREFIX_OPERATOR("~"),
  /* TOKEN_QUESTION      */ INFIX(PREC_ASSIGNMENT, conditional),
  /* TOKEN_EQ            */ UNUSED,
  /* TOKEN_LT            */ INFIX_OPERATOR(PREC_COMPARISON, "<"),
  /* TOKEN_GT            */ INFIX_OPERATOR(PREC_COMPARISON, ">"),
  /* TOKEN_LTEQ          */ INFIX_OPERATOR(PREC_COMPARISON, "<="),
  /* TOKEN_GTEQ          */ INFIX_OPERATOR(PREC_COMPARISON, ">="),
  /* TOKEN_EQEQ          */ INFIX_OPERATOR(PREC_EQUALITY, "=="),
  /* TOKEN_BANGEQ        */ INFIX_OPERATOR(PREC_EQUALITY, "!="),
  /* TOKEN_BREAK         */ UNUSED,
  /* TOKEN_CONTINUE      */ UNUSED,
  /* TOKEN_CLASS         */ UNUSED,
  /* TOKEN_CONSTRUCT     */ { NULL, NULL, constructorSignature, PREC_NONE, NULL },
  /* TOKEN_ELSE          */ UNUSED,
  /* TOKEN_FALSE         */ PREFIX(boolean),
  /* TOKEN_FOR           */ UNUSED,
  /* TOKEN_FOREIGN       */ UNUSED,
  /* TOKEN_IF            */ UNUSED,
  /* TOKEN_IMPORT        */ UNUSED,
  /* TOKEN_AS            */ UNUSED,
  /* TOKEN_IN            */ UNUSED,
  /* TOKEN_IS            */ INFIX_OPERATOR(PREC_IS, "is"),
  /* TOKEN_NULL          */ PREFIX(null),
  /* TOKEN_RETURN        */ UNUSED,
  /* TOKEN_STATIC        */ UNUSED,
  /* TOKEN_SUPER         */ PREFIX(super_),
  /* TOKEN_THIS          */ PREFIX(this_),
  /* TOKEN_TRUE          */ PREFIX(boolean),
  /* TOKEN_VAR           */ UNUSED,
  /* TOKEN_WHILE         */ UNUSED,
  /* TOKEN_FIELD         */ PREFIX(field),
  /* TOKEN_STATIC_FIELD  */ PREFIX(staticField),
  /* TOKEN_NAME          */ { name, NULL, namedSignature, PREC_NONE, NULL },
  /* TOKEN_NUMBER        */ PREFIX(literal),
  /* TOKEN_STRING        */ PREFIX(literal),
  /* TOKEN_INTERPOLATION */ PREFIX(stringInterpolation),
  /* TOKEN_LINE          */ UNUSED,
  /* TOKEN_ERROR         */ UNUSED,
  /* TOKEN_EOF           */ UNUSED
};

// The main entrypoint for the top-down operator precedence parser.
void parsePrecedence(CompilerBase* compiler, Precedence precedence)
{
  nextToken(compiler->parser);
  GrammarFn prefix = getRule(compiler->parser->previous.type)->prefix;

  if (prefix == NULL)
  {
    error(compiler->parser, "Expected expression.");
    return;
  }

  // Track if the precendence of the surrounding expression is low enough to
  // allow an assignment inside this one. We can't compile an assignment like
  // a normal expression because it requires us to handle the LHS specially --
  // it needs to be an lvalue, not an rvalue. So, for each of the kinds of
  // expressions that are valid lvalues -- names, subscripts, fields, etc. --
  // we pass in whether or not it appears in a context loose enough to allow
  // "=". If so, it will parse the "=" itself and handle it appropriately.
  bool canAssign = precedence <= PREC_CONDITIONAL;
  prefix(compiler, canAssign);

  while (precedence <= getRule(compiler->parser->current.type)->precedence)
  {
    nextToken(compiler->parser);
    GrammarFn infix = getRule(compiler->parser->previous.type)->infix;
    infix(compiler, canAssign);
  }
}

// Gets the [GrammarRule] associated with tokens of [type].
GrammarRule* getRule(TokenType type)
{
  return &rules[type];
}

bool matchAttribute(CompilerBase* compiler)
{
  if (!match(compiler->parser, TOKEN_HASH))
  {
    return false;
  }

  bool runtimeAccess = match(compiler->parser, TOKEN_BANG);
  if (match(compiler->parser, TOKEN_NAME))
  {
    Value group = compiler->parser->previous.value;
    TokenType ahead = peek(compiler->parser);
    if (ahead == TOKEN_EQ || ahead == TOKEN_LINE)
    {
      Value key = group;
      Value value = NULL_VAL;
      if (match(compiler->parser, TOKEN_EQ))
      {
        value = consumeLiteral(compiler->parser, "Expect a Bool, Num, String or Identifier literal for an attribute value.");
      }
      compiler->ops->attributeDefinition(compiler, runtimeAccess, NULL_VAL, key, value);
    }
    else if (match(compiler->parser, TOKEN_LEFT_PAREN))
    {
      ignoreNewlines(compiler->parser);
      if (match(compiler->parser, TOKEN_RIGHT_PAREN))
      {
        error(compiler->parser, "Expected attributes in group, group cannot be empty.");
      }
      else
      {
        while (peek(compiler->parser) != TOKEN_RIGHT_PAREN)
        {
          consume(compiler->parser, TOKEN_NAME, "Expect name for attribute key.");
          Value key = compiler->parser->previous.value;
          Value value = NULL_VAL;
          if (match(compiler->parser, TOKEN_EQ))
          {
            value = consumeLiteral(compiler->parser, "Expect a Bool, Num, String or Identifier literal for an attribute value.");
          }
          compiler->ops->attributeDefinition(compiler, runtimeAccess, group, key, value);
          ignoreNewlines(compiler->parser);
          if (!match(compiler->parser, TOKEN_COMMA)) break;
          ignoreNewlines(compiler->parser);
        }

        ignoreNewlines(compiler->parser);
        consume(compiler->parser, TOKEN_RIGHT_PAREN,
          "Expected ')' after grouped attributes.");
      }
    }
    else
    {
      error(compiler->parser, "Expect an equal, newline or grouping after an attribute key.");
    }
  }
  else
  {
    error(compiler->parser, "Expect an attribute definition after #.");
  }

  consumeLine(compiler->parser, "Expect newline after attribute.");
  return true;
}

// Parses + compiles a simple statement. These can only appear at the top-level or
// within curly blocks. Simple statements exclude variable binding statements
// like "var" and "class" which are not allowed directly in places like the
// branches of an "if" statement.
//
// Unlike expressions, statements do not leave a value on the stack.
void statement(CompilerBase* compiler)
{
  if (match(compiler->parser, TOKEN_BREAK))
  {
    compiler->ops->breakStatement(compiler);
  }
  else if (match(compiler->parser, TOKEN_CONTINUE))
  {
    compiler->ops->continueStatement(compiler);
  }
  else if (match(compiler->parser, TOKEN_FOR))
  {
    compiler->ops->forStatement(compiler);
  }
  else if (match(compiler->parser, TOKEN_IF))
  {
    compiler->ops->ifStatement(compiler);
  }
  else if (match(compiler->parser, TOKEN_RETURN))
  {
    compiler->ops->returnStatement(compiler);
  }
  else if (match(compiler->parser, TOKEN_WHILE))
  {
    compiler->ops->whileStatement(compiler);
  }
  else if (match(compiler->parser, TOKEN_LEFT_BRACE))
  {
    compiler->ops->blockStatement(compiler);
  }
  else
  {
    compiler->ops->expressionStatement(compiler);
  }
}

// Compiles a "definition". These are the statements that bind new variables.
// They can only appear at the top level of a block and are prohibited in places
// like the non-curly body of an if or while.
void definition(CompilerBase* compiler)
{
  // consume attribute(s)
  bool hasAttribute = matchAttribute(compiler);
  while (matchAttribute(compiler)) {};

  if (match(compiler->parser, TOKEN_CLASS))
  {
    compiler->ops->classDefinition(compiler, false);
    return;
  }
  else if (match(compiler->parser, TOKEN_FOREIGN))
  {
    consume(compiler->parser, TOKEN_CLASS, "Expect 'class' after 'foreign'.");
    compiler->ops->classDefinition(compiler, true);
    return;
  }

  // Throw an error if any attributes were found preceding,
  // and clear the attributes so the error doesn't keep happening.
  if (hasAttribute)
  {
    error(compiler->parser, "Attributes can only specified before a class or a method");
    compiler->ops->discardAttributes(compiler);
  }

  if (match(compiler->parser, TOKEN_IMPORT))
  {
    compiler->ops->import(compiler);
  }
  else if (match(compiler->parser, TOKEN_VAR))
  {
    compiler->ops->variableDefinition(compiler);
  }
  else
  {
    statement(compiler);
  }
}

// Parses an expression. Unlike statements, expressions leave a resulting value
// on the stack.
void expression(CompilerBase* compiler)
{
  parsePrecedence(compiler, PREC_LOWEST);
}

// Returns a signature with [type] whose name is from the last consumed token.
Signature signatureFromToken(Parser* parser, SignatureType type)
{
  Signature signature;

  // Get the token for the method name.
  Token* token = &parser->previous;
  signature.name = token->start;
  signature.length = token->length;
  signature.type = type;
  signature.arity = 0;

  if (signature.length > MAX_METHOD_NAME)
  {
    error(parser, "Method names cannot be longer than %d characters.",
          MAX_METHOD_NAME);
    signature.length = MAX_METHOD_NAME;
  }

  return signature;
}

// The VM can only handle a certain number of parameters, so check that we
// haven't exceeded that and give a usable error.
void validateNumParameters(Parser* parser, int numArgs)
{
  if (numArgs == MAX_PARAMETERS + 1)
  {
    // Only show an error at exactly max + 1 so that we can keep parsing the
    // parameters and minimize cascaded errors.
    error(parser, "Methods cannot have more than %d parameters.",
          MAX_PARAMETERS);
  }
}

// Declares a variable in the current scope whose name is the given token.
//
// If [token] is `NULL`, uses the previously consumed token. Returns its symbol.
int declareVariable(CompilerBase* compiler, Token* token)
{
  if (token == NULL) token = &compiler->parser->previous;

  if (token->length > MAX_VARIABLE_NAME)
  {
    error(compiler->parser, "Variable name cannot be longer than %d characters.",
          MAX_VARIABLE_NAME);
  }

  // Top-level module scope.
  if (compiler->scopeDepth == -1)
  {
    return compiler->ops->defineModuleVariable(compiler, token);
  }

  // See if there is already a variable with this name declared in the current
  // scope. (Outer scopes are OK: those get shadowed.)
  for (int i = compiler->numLocals - 1; i >= 0; i--)
  {
    Local* local = &compiler->locals[i];

    // Once we escape this scope and hit an outer one, we can stop.
    if (local->depth < compiler->scopeDepth) break;

    if (local->length == token->length &&
        memcmp(local->name, token->start, token->length) == 0)
    {
      error(compiler->parser, "Variable is already declared in this scope.");
      return i;
    }
  }

  if (compiler->numLocals == MAX_LOCALS)
  {
    error(compiler->parser, "Cannot declare more than %d variables in one scope.",
          MAX_LOCALS);
    return -1;
  }

  return addLocal(compiler, token->start, token->length);
}

// Parses a name token and declares a variable in the current scope with that
// name. Returns its slot.
int declareNamedVariable(CompilerBase* compiler)
{
  consume(compiler->parser, TOKEN_NAME, "Expect variable name.");
  return declareVariable(compiler, NULL);
}

// Parses the rest of a comma-separated parameter list after the opening
// delimeter. Updates [arity] with the number of parameters.
void finishParameterList(CompilerBase* compiler, int* arity)
{
  do
  {
    ignoreNewlines(compiler->parser);
    validateNumParameters(compiler->parser, ++*arity);

    // Define a local variable in the method for the parameter.
    declareNamedVariable(compiler);
  }
  while (match(compiler->parser, TOKEN_COMMA));
}

// Create a new local variable with [name]. Assumes the current scope is local
// and the name is unique.
int addLocal(CompilerBase* compiler, const char* name, int length)
{
  Local* local = &compiler->locals[compiler->numLocals];
  local->name = name;
  local->length = length;
  local->depth = compiler->scopeDepth;
  local->isUpvalue = false;
  return compiler->numLocals++;
}

// Parses a block body, after the initial "{" has been consumed.
//
// Returns true if it was a expression body, false if it was a statement body.
// (More precisely, returns true if a value was left on the stack. An empty
// block returns false.)
bool finishBlock(CompilerBase* compiler)
{
  // Empty blocks do nothing.
  if (match(compiler->parser, TOKEN_RIGHT_BRACE)) return false;

  // If there's no line after the "{", it's a single-expression body.
  if (!matchLine(compiler->parser))
  {
    expression(compiler);
    consume(compiler->parser, TOKEN_RIGHT_BRACE, "Expect '}' at end of block.");
    return true;
  }

  // Empty blocks (with just a newline inside) do nothing.
  if (match(compiler->parser, TOKEN_RIGHT_BRACE)) return false;

  // Compile the definition list.
  do
  {
    definition(compiler);
    consumeLine(compiler->parser, "Expect newline after statement.");
  }
  while (peek(compiler->parser) != TOKEN_RIGHT_BRACE && peek(compiler->parser) != TOKEN_EOF);

  consume(compiler->parser, TOKEN_RIGHT_BRACE, "Expect '}' at end of block.");
  return false;
}

// Parses a comma-separated list of arguments. Increments [arity] to match
// the arity of the argument list.
void finishArgumentList(CompilerBase* compiler, int* arity)
{
  do
  {
    ignoreNewlines(compiler->parser);
    validateNumParameters(compiler->parser, ++*arity);
    expression(compiler);
  }
  while (match(compiler->parser, TOKEN_COMMA));

  // Allow a newline before the closing delimiter.
  ignoreNewlines(compiler->parser);
}

// Starts a new local block scope.
void pushScope(CompilerBase* compiler)
{
  compiler->scopeDepth++;
}

// Closes the last pushed block scope and discards any local variables declared
// in that scope. This should only be called in a statement context where no
// temporaries are still on the stack.
void popScope(CompilerBase* compiler)
{
  int popped = compiler->ops->discardLocals(compiler, compiler->scopeDepth);
  compiler->numLocals -= popped;
  compiler->numSlots -= popped;
  compiler->scopeDepth--;
}

// Attempts to look up the name in the local variables of [compiler]. If found,
// returns its index, otherwise returns -1.
int resolveLocal(CompilerBase* compiler, const char* name, int length)
{
  // Look it up in the local scopes. Look in reverse order so that the most
  // nested variable is found first and shadows outer ones.
  for (int i = compiler->numLocals - 1; i >= 0; i--)
  {
    if (compiler->locals[i].length == length &&
        memcmp(name, compiler->locals[i].name, length) == 0)
    {
      return i;
    }
  }

  return -1;
}
