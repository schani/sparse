#ifndef EXPRESSION_H
#define EXPRESSION_H
/*
 * sparse/expression.h
 *
 * Copyright (C) 2003 Linus Torvalds, all rights reserved
 *
 * Declarations and helper functions for expression parsing.
 */

enum expression_type {
	EXPR_CONSTANT,
	EXPR_VALUE,
	EXPR_SYMBOL,
	EXPR_BINOP,
	EXPR_DEREF,
	EXPR_PREOP,
	EXPR_POSTOP,
	EXPR_CAST,
	EXPR_SIZEOF,
	EXPR_CONDITIONAL,
	EXPR_STATEMENT,
	EXPR_CALL,
};

struct expression {
	int type, op;
	struct token *token;
	struct symbol *ctype;
	union {
		long long value;
		struct expression *unop;
		struct statement *statement;
		struct symbol *symbol;
		struct binop_arg {
			struct expression *left, *right;
		};
		struct deref_arg {
			struct expression *deref;
			struct token *member;
		};
		struct cast_arg {
			struct symbol *cast_type;
			struct expression *cast_expression;
		};
		struct conditional_expr {
			struct expression *conditional, *cond_true, *cond_false;
		};
		struct statement_struct {
			struct symbol_list *syms;
			struct statement_list *stmts;
		};
	};
};

/* Constant expression values */
long long get_expression_value(struct expression *);

/* Expression parsing */
struct token *parse_expression(struct token *token, struct expression **tree);
struct token *conditional_expression(struct token *token, struct expression **tree);
struct token *primary_expression(struct token *token, struct expression **tree);
struct token *parens_expression(struct token *token, struct expression **expr, const char *where);
struct token *assignment_expression(struct token *token, struct expression **tree);

extern int evaluate_expression(struct expression *);

static inline struct expression *alloc_expression(struct token *token, int type)
{
	struct expression *expr = __alloc_expression(0);
	expr->type = type;
	expr->token = token;
	return expr;
}

/* Type name parsing */
struct token *typename(struct token *, struct symbol **);

static inline int lookup_type(struct token *token)
{
	if (token->type == TOKEN_IDENT)
		return lookup_symbol(token->ident, NS_TYPEDEF) != NULL;
	return 0;
}

/* Statement parsing */
struct statement *alloc_statement(struct token * token, int type);
struct token *initializer(struct token *token, struct ctype *type);
struct token *compound_statement(struct token *, struct statement *);

/* The preprocessor calls this 'constant_expression()' */
#define constant_expression(token,tree) conditional_expression(token, tree)

#endif