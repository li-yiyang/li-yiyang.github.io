#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <readline/readline.h>
#include <readline/history.h>

#define TRUE  1
#define FALSE 0

// the SYMBOL_BUFF_SIZE should longer than 3 (nil)
#define SYMBOL_BUFF_SIZE 16

#define WHITESPACEP(chr)                                                       \
  ((chr) == ' ' || (chr) == '\t' || (chr) == '\r' || (chr) == '\n')

#define DEBUG(str, pos, func, message)                        \
  {                                                           \
    fprintf(stderr, func);                                    \
    putc('\n', stderr);                                       \
    fprintf(stderr, "%s\n", str);                             \
    for (size_t i = 0; i < pos; i++) putc(' ', stderr);       \
    putc('^', stderr);                                        \
    putc('\n', stderr);                                       \
    fprintf(stderr, message);                                 \
    putc('\n', stderr);                                       \
  }

#ifdef DBG_LEVEL1
int debug1_depth_ = 0;
#define INIT_CALL debug1_depth_ = 0;

#define FUNC_CALL(func)                         \
  {                                             \
    debug1_depth_++;                            \
    fprintf(stderr, ">> [%d] ", debug1_depth_); \
    fprintf(stderr, func);                      \
    putc('\n', stderr);                         \
  }

#define FUNC_RET(res)                                     \
  {                                                       \
    debug1_depth_--;                                      \
    fprintf(stderr, "<< [%d]\n", debug1_depth_);          \
    return res;                                           \
  }

#define DEBUG1(str, pos, func, message)                     \
  {                                                         \
  fprintf(stderr, func);                                    \
  fprintf(stderr, " [%d]\n", debug1_depth_);                \
  fprintf(stderr, "%s\n", str);                             \
  for (size_t i = 0; i < pos; i++) putc(' ', stderr);       \
  putc('^', stderr);                                        \
  putc('\n', stderr);                                       \
  fprintf(stderr, message);                                 \
  putc('\n', stderr);                                       \
  }

#else
#define INIT_CALL
#define FUNC_CALL(func)
#define FUNC_RET(res) return res;
#define DEBUG1(str, pos, func, message)
#endif

#define SKIP_WHITESPACE(str, pos)               \
  while (WHITESPACEP(str[pos])) {               \
    DEBUG1(str, pos, "SKIP_WHITESPACE", "");    \
    if (str[pos] != '\0')                       \
      pos++;                                    \
  }

// ========================= Object DataStruct ==========================

typedef struct symbol_name_ {
  size_t size;
  char name[SYMBOL_BUFF_SIZE];
  struct symbol_name_ *rest;
} SymbolName;

typedef struct object_ Object;

typedef struct cons_ {
  Object* car;
  Object* cdr;
} Cons;

struct object_ {
  enum { CONS, SYMBOL } tag;

  union {
    Cons        *cons;
    SymbolName  *symbol;
  } val;
};

/*
  Trun CAR and CDR as Object cons.
  Return a pointer to cons Object.
 */
Object *cons(Object *car, Object *cdr) {
  Object *cons = malloc(sizeof(Object));
  cons->tag = CONS;
  cons->val.cons = malloc(sizeof(Cons));
  cons->val.cons->car = car;
  cons->val.cons->cdr = cdr;
  return cons;
}

// ========================= Read Object ==========================

Object *read_from_string_ (char *str, size_t *pos);

/*
  Read SymbolName from STR starting from POS.
  Return pointer to SymbolName object.

  Parameters:
  + POS should point to the start of symbol in STR.

    Example:

      ( foo bar ... )
        ^
       pos

      ( foooooooooooooooooooooobaaaaaaaaar ... )
        [ old-buffer ]
                      ^
                     pos

    After read_from_string_, POS should point to next position
    of the symbol.

    Example

      ( foo bar ... )
           ^
          pos
 */
SymbolName *read_SymbolName_from_string_(char *str, size_t *pos) {
  FUNC_CALL("read_SymbolName_from_string_");

  char token = str[*pos];

  if (WHITESPACEP(token) || token == '\0' || token == '(' || token == ')')
    FUNC_RET(NULL);

  SymbolName *sym = malloc(sizeof(SymbolName));
  size_t size = 0;

  do {
    DEBUG1(str, (*pos), "read_SymbolName_from_string_", "");
    sym->name[size++] = token;
    token = str[++(*pos)];
  } while (size < SYMBOL_BUFF_SIZE && !WHITESPACEP(token) &&
           token != '\0' && token != '(' && token != ')');

  if (size < SYMBOL_BUFF_SIZE) {
    sym->rest = NULL;
    sym->size = size;
  } else {
    sym->rest = read_SymbolName_from_string_(str, pos);
    sym->size = (sym->rest == NULL ? 0 : sym->rest->size) + SYMBOL_BUFF_SIZE;
  }

  FUNC_RET(sym);
}

/*
  Read Cons from STR starting from POS.
  Return pointer to Cons object.

  Parameters:
  + POS should point to the STR next to the ( mark.

    Example:

      ( foo bar ... )
        ^
       pos

    After read_cons_from_string_, POS should point to next position of
    the list.

    Example:

      ( foo bar ... )
                     ^
                    pos
 */
Object *read_cons_from_string_(char *str, size_t *pos) {
  FUNC_CALL("read_cons_from_string_");
  SKIP_WHITESPACE(str, (*pos));

  if (str[*pos] == '\0') {
    DEBUG(str, (*pos), "read_cons_from_string_", "End of input. ");
    FUNC_RET(NULL);
  }

  if (str[*pos] == ')') {
    DEBUG1(str, (*pos), "read_cons_from_string_", "End of cons ). \n");
    (*pos)++;
    FUNC_RET(NULL);
  }

  Object *car = read_from_string_(str, pos);
  Object *cdr = read_cons_from_string_(str, pos);
  Object *obj = cons(car, cdr);

  FUNC_RET(obj);
}

/*
  Read Object from string STR, starting from POS.
 */
Object *read_from_string_(char *str, size_t *pos) {
  FUNC_CALL("read_from_string_");
  Object *obj;

  SKIP_WHITESPACE(str, (*pos));

  switch (str[*pos]) {
  case '\0':
    DEBUG(str, (*pos), "read_from_string_", "End of input line. ");
    free(obj);
    FUNC_RET(NULL);
    break;

  case '(':
    DEBUG1(str, (*pos), "read_from_string_", "read list cons mark ( \n");
    (*pos)++; // skip (
    obj = read_cons_from_string_(str, pos);
    break;

  default:
    obj = malloc(sizeof(Object));
    obj->tag = SYMBOL;
    obj->val.symbol = read_SymbolName_from_string_(str, pos);

    // failed to read SymbolNames
    if (obj->val.symbol == NULL) {
      free(obj);
      FUNC_RET(NULL);
    }

    break;
  }

  FUNC_RET(obj);
}

/*
  Read Object from string STR.
  Return a pointer to the Object.

  See `read_from_string_` for implementation.
  This should be equal to read_from_string_(str, &0).
 */
Object *read_from_string(char *str) {
  size_t pos = 0;
  INIT_CALL;
  return read_from_string_(str, &pos);
}

// ========================= Print Object ==========================

void print_SymbolName(SymbolName *sym, FILE *stream) {
  if (sym == NULL)
    return;

  for (size_t i = 0; i < sym->size && i < SYMBOL_BUFF_SIZE; i++)
    putc(sym->name[i], stream);

  print_SymbolName(sym->rest, stream);
}

void print_object(Object *obj, FILE *stream) {
  if (obj == NULL) {
    fprintf(stream, "nil");
    return;
  }

  switch (obj->tag) {
  case CONS:
    // TODO: better print CONS if its a list
    // (a . (b . nil)) -> (a b)
    putc('(', stream);
    print_object(obj->val.cons->car, stream);
    putc(' ', stream);
    putc('.', stream);
    putc(' ', stream);
    print_object(obj->val.cons->cdr, stream);
    putc(')', stream);
    break;
  case SYMBOL:
    print_SymbolName(obj->val.symbol, stream);
    break;
  }
}

// ========================= Deallocation ==========================

void free_Object(Object *obj);

/*
  Free SymbolName Object SYM.
 */
void free_SymbolName(SymbolName *sym) {
  if (sym->rest != NULL)
    free_SymbolName(sym->rest);

  free(sym);
}

/*
  Free Cons object CONS.
 */
void free_Cons(Cons *cons) {
  if (cons->car != NULL)
    free_Object(cons->car);
  if (cons->cdr != NULL)
    free_Object(cons->cdr);
  free(cons);
}

void free_Object(Object *obj) {
  if (obj == NULL)
    return;

  switch (obj->tag) {
  case CONS:
    free_Cons(obj->val.cons);
    break;
  case SYMBOL:
    free_SymbolName(obj->val.symbol);
    break;
  }

  free(obj);
}

// ========================= Main ==========================

int main(int argc, char **argv) {
  size_t pos = 0;
  char *line;
  Object *obj;

  while ((line = readline("> ")) != NULL) {
    obj = read_from_string(line);
    print_object(obj, stdout);
    putc('\n', stdout);
    free_Object(obj);
  }

}
