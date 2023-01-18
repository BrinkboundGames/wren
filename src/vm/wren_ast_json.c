
#include "wren_ast_json.h"
#include "wren_ast.h"
#include "json-c/json.h"

static void prepared_line_file(const char* line, const char* output)
{
  FILE* fptr;
  fptr = fopen(output, "a");
  fwrite(line, sizeof(char), strlen(line), fptr);

  // The file should be closed when everything is written
  fclose(fptr);
}

json_object* createLocation(Token* token)
{
  json_object* location = json_object_new_object();
  json_object* col = json_object_new_int(0); // TODO
  json_object* line = json_object_new_int(token->line);
  json_object_object_add(location, "col", col);
  json_object_object_add(location, "line", line);
  return location;
}

json_object* importStmtToJsonObject(Stmt* stmt)
{
  json_object* body = json_object_new_object();
  AliasNode* curr = stmt->op.importStmt.variables;
  while (curr)
  {
    json_object* obj = json_object_new_object();

    // location
    json_object_object_add(obj, "location", createLocation(&curr->val.val));

    // name
    json_object* name = json_object_new_string_len(curr->val.alias.start, curr->val.alias.length);
    json_object_object_add(obj, "name", name);

    // source
    json_object* source = json_object_new_string_len(curr->val.val.start, curr->val.val.length);
    json_object_object_add(obj, "source", source);

    // append onto body obj
    json_object_object_add(body, json_object_get_string(name), obj);

    curr = curr->next;
  }

  json_object* import = NULL;

  int len = stmt->op.importStmt.path.length + 1;
  char* key = malloc(len);
  if (key)
  {
    strncpy(key, stmt->op.importStmt.path.start, stmt->op.importStmt.path.length);
    import = json_object_new_object();
    json_object_object_add(import, key, body);
    free(key);
  }

  return import;
}

json_object* stmtToJsonObject(Stmt* stmt)
{
  json_object* obj = NULL;

  switch (stmt->tag)
  {
  case ForStmt:
    break;
  case ReturnStmt:
      break;
  case BlockStmt:
      break;
  case VarStmt:
      break;
  case ImportStmt:
      obj = importStmtToJsonObject(stmt);
      break;
  case IfStmt:
      break;
  case BreakStmt:
      break;
  case ContinueStmt:
      break;
  case WhileStmt:
      break;
  case ClassStmt:
      break;
  case ExprStmt:
      break;
  }

  return obj;
}

void printAstStatementsToJSON(astCompiler* compiler, const char* output)
{
  json_object * root = json_object_new_object();
  json_object* imports = json_object_new_object();

  StmtNode* curr = compiler->statementStack;
  while (curr)
  {
    json_object* obj = stmtToJsonObject(curr->val);
    if (obj)
    {
        struct lh_entry* entry = json_object_get_object(obj)->head;
        if (entry)
        {
            json_object_object_add(imports, (char*)entry->k, (struct json_object*)entry->v);
        }
    }
    curr = curr->next;
  }

  json_object_object_add(root, "imports", imports);

  // Transforms the binary representation into a string representation
  const char* serialized_json = json_object_to_json_string(root);

  // print to the "output" file
  prepared_line_file(serialized_json, output);
}