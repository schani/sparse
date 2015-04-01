/*
 * Example trivial client program that uses the sparse library
 * to tokenize, preprocess and parse a C file, and prints out
 * the results.
 *
 * Copyright (C) 2003 Transmeta Corp.
 *               2003-2004 Linus Torvalds
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <fcntl.h>
#include <assert.h>

#include "lib.h"
#include "allocate.h"
#include "token.h"
#include "parse.h"
#include "symbol.h"
#include "expression.h"
#include "linearize.h"

static int context_increase(struct basic_block *bb, int entry)
{
	int sum = 0;
	struct instruction *insn;

	FOR_EACH_PTR(bb->insns, insn) {
		int val;
		if (insn->opcode != OP_CONTEXT)
			continue;
		val = insn->increment;
		if (insn->check) {
			int current = sum + entry;
			if (!val) {
				if (!current)
					continue;
			} else if (current >= val)
				continue;
			warning(insn->pos, "context check failure");
			continue;
		}
		sum += val;
	} END_FOR_EACH_PTR(insn);
	return sum;
}

static int imbalance(struct entrypoint *ep, struct basic_block *bb, int entry, int exit, const char *why)
{
	if (Wcontext) {
		struct symbol *sym = ep->name;
		warning(bb->pos, "context imbalance in '%s' - %s", show_ident(sym->ident), why);
	}
	return -1;
}

static int check_bb_context(struct entrypoint *ep, struct basic_block *bb, int entry, int exit);

static int check_children(struct entrypoint *ep, struct basic_block *bb, int entry, int exit)
{
	struct instruction *insn;
	struct basic_block *child;

	insn = last_instruction(bb->insns);
	if (!insn)
		return 0;
	if (insn->opcode == OP_RET)
		return entry != exit ? imbalance(ep, bb, entry, exit, "wrong count at exit") : 0;

	FOR_EACH_PTR(bb->children, child) {
		if (check_bb_context(ep, child, entry, exit))
			return -1;
	} END_FOR_EACH_PTR(child);
	return 0;
}

static int check_bb_context(struct entrypoint *ep, struct basic_block *bb, int entry, int exit)
{
	if (!bb)
		return 0;
	if (bb->context == entry)
		return 0;

	/* Now that's not good.. */
	if (bb->context >= 0)
		return imbalance(ep, bb, entry, bb->context, "different lock contexts for basic block");

	bb->context = entry;
	entry += context_increase(bb, entry);
	if (entry < 0)
		return imbalance(ep, bb, entry, exit, "unexpected unlock");

	return check_children(ep, bb, entry, exit);
}

static void check_cast_instruction(struct instruction *insn)
{
	struct symbol *orig_type = insn->orig_type;
	if (orig_type) {
		int old = orig_type->bit_size;
		int new = insn->size;
		int oldsigned = (orig_type->ctype.modifiers & MOD_SIGNED) != 0;
		int newsigned = insn->opcode == OP_SCAST;

		if (new > old) {
			if (oldsigned == newsigned)
				return;
			if (newsigned)
				return;
			warning(insn->pos, "cast loses sign");
			return;
		}
		if (new < old) {
			warning(insn->pos, "cast drops bits");
			return;
		}
		if (oldsigned == newsigned) {
			warning(insn->pos, "cast wasn't removed");
			return;
		}
		warning(insn->pos, "cast changes sign");
	}
}

static void check_range_instruction(struct instruction *insn)
{
	warning(insn->pos, "value out of range");
}

static void check_byte_count(struct instruction *insn, pseudo_t count)
{
	if (!count)
		return;
	if (count->type == PSEUDO_VAL) {
		long long val = count->value;
		if (val <= 0 || val > 100000)
			warning(insn->pos, "%s with byte count of %lld",
				show_ident(insn->func->sym->ident), val);
		return;
	}
	/* OK, we could try to do the range analysis here */
}

static pseudo_t argument(struct instruction *call, unsigned int argno)
{
	pseudo_t args[8];
	struct ptr_list *arg_list = (struct ptr_list *) call->arguments;

	argno--;
	if (linearize_ptr_list(arg_list, (void *)args, 8) > argno)
		return args[argno];
	return NULL;
}

static void check_memset(struct instruction *insn)
{
	check_byte_count(insn, argument(insn, 3));
}

#define check_memcpy check_memset
#define check_ctu check_memset
#define check_cfu check_memset

struct checkfn {
	struct ident *id;
	void (*check)(struct instruction *insn);
};

static void check_call_instruction(struct instruction *insn)
{
	pseudo_t fn = insn->func;
	struct ident *ident;
	static const struct checkfn check_fn[] = {
		{ &memset_ident, check_memset },
		{ &memcpy_ident, check_memcpy },
		{ &copy_to_user_ident, check_ctu },
		{ &copy_from_user_ident, check_cfu },
	};
	int i;

	if (fn->type != PSEUDO_SYM)
		return;
	ident = fn->sym->ident;
	if (!ident)
		return;
	for (i = 0; i < ARRAY_SIZE(check_fn); i++) {
		if (check_fn[i].id != ident)
			continue;
		check_fn[i].check(insn);
		break;
	}
}

static void check_one_instruction(struct instruction *insn)
{
	switch (insn->opcode) {
	case OP_CAST: case OP_SCAST:
		if (verbose)
			check_cast_instruction(insn);
		break;
	case OP_RANGE:
		check_range_instruction(insn);
		break;
	case OP_CALL:
		check_call_instruction(insn);
		break;
	default:
		break;
	}
}

static void check_bb_instructions(struct basic_block *bb)
{
	struct instruction *insn;
	FOR_EACH_PTR(bb->insns, insn) {
		if (!insn->bb)
			continue;
		check_one_instruction(insn);
	} END_FOR_EACH_PTR(insn);
}

static void check_instructions(struct entrypoint *ep)
{
	struct basic_block *bb;
	FOR_EACH_PTR(ep->bbs, bb) {
		check_bb_instructions(bb);
	} END_FOR_EACH_PTR(bb);
}

static void check_context(struct entrypoint *ep)
{
	struct symbol *sym = ep->name;
	struct context *context;
	unsigned int in_context = 0, out_context = 0;

	if (Wuninitialized && verbose && ep->entry->bb->needs) {
		pseudo_t pseudo;
		FOR_EACH_PTR(ep->entry->bb->needs, pseudo) {
			if (pseudo->type != PSEUDO_ARG)
				warning(sym->pos, "%s: possible uninitialized variable (%s)",
					show_ident(sym->ident), show_pseudo(pseudo));
		} END_FOR_EACH_PTR(pseudo);
	}

	check_instructions(ep);

	FOR_EACH_PTR(sym->ctype.contexts, context) {
		in_context += context->in;
		out_context += context->out;
	} END_FOR_EACH_PTR(context);
	check_bb_context(ep, ep->entry->bb, in_context, out_context);
}

static struct permission_list* gather_permissions (struct symbol *sym)
{
	struct permission_list *list = NULL;
	/*
	 * FIXME: We shouldn't have to do this hack, but sometimes the
	 * type of a function expression has the permission only in
	 * its base type, such as when the function is a member of a
	 * struct.
	 */
	for (;;) {
		struct permission *perm;
		concat_ptr_list((struct ptr_list *)sym->ctype.permissions,
				(struct ptr_list **)&list);

		if (sym->type == SYM_NODE)
			sym = sym->ctype.base_type;
		else if (sym->type == SYM_PTR)
			sym = get_base_type(sym);
		else
			break;
	}
	return list;
}

static int idents_equal(struct ident *i1, struct ident *i2)
{
	if (i1->len != i2->len)
		return 0;
	return !strncmp(i1->name, i2->name, i1->len);
}

static int has_permission(struct permission_list *list, struct permission *permission)
{
	struct permission *sym_permission;
	FOR_EACH_PTR(list, sym_permission) {
		if (idents_equal(permission->name, sym_permission->name))
			return 1;
	} END_FOR_EACH_PTR(sym_permission);
	return 0;
}

static void check_call_symbol_permission(struct position pos, struct symbol *caller, struct symbol *callee)
{
	struct permission_list *caller_list;
	struct permission_list *callee_list;
	struct permission *caller_permission;

	if (!caller || !callee)
		return;

	caller_list = gather_permissions(caller);
	callee_list = gather_permissions(callee);

	FOR_EACH_PTR(caller_list, caller_permission) {
		if (!has_permission(callee_list, caller_permission)) {
			warning(pos, "function %s", show_ident(caller->ident));
			if (callee->ident)
				warning(pos, "  calls function %s", show_ident(callee->ident));
			else
				warning(pos, "  call function pointer");
			warning(pos, "  without permission %s", show_ident(caller_permission->name));
		}
	} END_FOR_EACH_PTR(caller_permission);

	free_ptr_list(&caller_list);
	free_ptr_list(&callee_list);
}

static void check_permission_initializer(struct expression *initializer, struct symbol *containing_fn);
static void check_permission_expression(struct expression *expr, struct symbol *containing_fn);
static void check_permission_statement(struct statement *stmt, struct symbol *containing_fn);

static void check_permission_position(struct expression *pos, struct symbol *containing_fn)
{
	struct expression *init_expr = pos->init_expr;
	check_permission_initializer(init_expr, containing_fn);
}

static void check_permission_initializer(struct expression *initializer, struct symbol *containing_fn)
{
	switch (initializer->type) {
	case EXPR_INITIALIZER: {
		struct expression *expr;
		FOR_EACH_PTR(initializer->expr_list, expr) {
			check_permission_initializer(expr, containing_fn);
		} END_FOR_EACH_PTR(expr);
		break;
	}
	case EXPR_POS:
		check_permission_position(initializer, containing_fn);
		break;
	default:
		check_permission_expression(initializer, containing_fn);
		break;
	}
}

static void check_permission_one_symbol(struct symbol *sym, struct symbol *containing_fn)
{
	if (!sym->initializer)
		return;
	check_permission_initializer(sym->initializer, containing_fn);
}

static void check_permission_declaration(struct statement *stmt, struct symbol *containing_fn)
{
	struct symbol *sym;
	FOR_EACH_PTR(stmt->declaration, sym) {
		check_permission_one_symbol(sym, containing_fn);
	} END_FOR_EACH_PTR(sym);
}

static struct symbol* get_rhs_symbol (struct expression *src)
{
	if (src->type == EXPR_SYMBOL) {
		return src->symbol;
	} else if (src->type == EXPR_PREOP && src->op == '*') {
		if (src->unop->type == EXPR_SYMBOL)
			return src->unop->symbol;
	}
	return src->ctype;
}

static void check_permission_assignment(struct expression *expr, struct symbol *containing_fn)
{
	struct expression *target = expr->left;
	struct expression *src = expr->right;
	struct symbol *src_symbol = NULL;
	struct symbol *target_symbol = NULL;

	check_permission_expression(src, containing_fn);
	check_permission_expression(target, containing_fn);

	if (!src || !target)
		return;

	src_symbol = get_rhs_symbol(src);

	if (target->type == EXPR_PREOP && target->op == '*') {
		if (target->unop->type == EXPR_SYMBOL) {
			target_symbol = target->unop->symbol;
		} else if (target->unop->type == EXPR_BINOP && target->unop->op == '+') {
			target_symbol = target->ctype;
		}
	}

	if (src_symbol && target_symbol) {
		check_call_symbol_permission(expr->pos, target_symbol, src_symbol);
	} else {
		//warning(expr->pos, "could not get type information for assignment");
	}
}

static void check_permission_call_expression(struct expression *expr, struct symbol *containing_fn)
{
	struct expression *arg;
	struct symbol *callee_symbol = expr->fn->ctype;
	struct symbol *fn_symbol, *argtype;
	struct symbol_list *argument_types;

	fn_symbol = callee_symbol;
	if (!fn_symbol)
		return;

	if (fn_symbol->type == SYM_NODE)
		fn_symbol = fn_symbol->ctype.base_type;
	if (fn_symbol->type == SYM_PTR)
		fn_symbol = get_base_type(fn_symbol);
	if (fn_symbol->type != SYM_FN) {
		sparse_error(expr->pos, "not a function %s", show_ident(callee_symbol->ident));
		return;
	}
	argument_types = fn_symbol->arguments;

	PREPARE_PTR_LIST(argument_types, argtype);
	FOR_EACH_PTR(expr->args, arg) {
		struct symbol *arg_symbol = get_rhs_symbol(arg);

		check_permission_expression(arg, containing_fn);

		if (arg_symbol)
			check_call_symbol_permission(arg->pos, argtype, arg_symbol);
		else
			warning(arg->pos, "can't check argument types");

		NEXT_PTR_LIST(argtype);
	} END_FOR_EACH_PTR(arg);
	FINISH_PTR_LIST(argtype);

	if (callee_symbol) {
		check_call_symbol_permission(expr->pos, containing_fn, callee_symbol);
	} else {
		check_permission_expression(expr->fn, containing_fn);
		warning(expr->pos, "function %s makes call to non-symbol function", show_ident(containing_fn->ident));
	}
}

static void check_permission_expression(struct expression *expr, struct symbol *containing_fn)
{
	if (!expr)
		return;

	switch (expr->type) {
	case EXPR_SYMBOL:
		if (expr->symbol)
			check_permission_one_symbol(expr->symbol, containing_fn);
		break;

	case EXPR_VALUE: case EXPR_STRING: case EXPR_FVALUE: case EXPR_LABEL:
		break;

	case EXPR_STATEMENT:
		check_permission_statement(expr->statement, containing_fn);
		break;

	case EXPR_CALL:
		check_permission_call_expression(expr, containing_fn);
		break;

	case EXPR_BINOP:
	case EXPR_LOGICAL:
	case EXPR_COMPARE:
	case EXPR_COMMA:
		check_permission_expression(expr->left, containing_fn);
		check_permission_expression(expr->right, containing_fn);
		break;

	case EXPR_SELECT:
		assert(expr->cond_true);
	case EXPR_CONDITIONAL:
		check_permission_expression(expr->conditional, containing_fn);
		check_permission_expression(expr->cond_true, containing_fn);
		check_permission_expression(expr->cond_false, containing_fn);
		break;

	case EXPR_ASSIGNMENT:
		check_permission_assignment(expr, containing_fn);
		break;

	case EXPR_PREOP:
	case EXPR_POSTOP:
		check_permission_expression(expr->unop, containing_fn);
		break;

	case EXPR_CAST:
	case EXPR_FORCE_CAST:
	case EXPR_IMPLIED_CAST:
		check_permission_expression(expr->cast_expression, containing_fn);
		break;

	case EXPR_SLICE:
		check_permission_expression(expr->base, containing_fn);
		break;

	case EXPR_INITIALIZER:
	case EXPR_POS:
		warning(expr->pos, "unexpected initializer expression (%d %d)", expr->type, expr->op);
		break;
	default:
		warning(expr->pos, "unknown expression (%d %d)", expr->type, expr->op);
		break;
	}
}

static void check_permission_asm_statement(struct statement *stmt, struct symbol *containing_fn)
{
	int state;
	struct expression *expr;

	FOR_EACH_PTR(stmt->asm_inputs, expr) {
		switch (state) {
		case 0:	/* Identifier */
			state = 1;
			continue;

		case 1:	/* Constraint */
			state = 2;
			continue;

		case 2:	/* Expression */
			state = 0;
			check_permission_expression(expr, containing_fn);
			continue;
		}
	} END_FOR_EACH_PTR(expr);

	FOR_EACH_PTR(stmt->asm_outputs, expr) {
		switch (state) {
		case 0:	/* Identifier */
			state = 1;
			continue;

		case 1:	/* Constraint */
			state = 2;
			continue;

		case 2:	/* Expression */
			state = 0;
			check_permission_expression(expr, containing_fn);
			continue;
		}
	} END_FOR_EACH_PTR(expr);
}

static struct symbol*
get_function_return_type (struct symbol *fn)
{
	if (fn->type == SYM_NODE)
		fn = fn->ctype.base_type;
	assert(fn->type == SYM_FN);
	return fn->ctype.base_type;
}

static void check_permission_return(struct statement *stmt, struct symbol *containing_fn)
{
	struct symbol *return_type, *value_symbol;

	if (!stmt->expression)
		return;

	return_type = get_function_return_type(containing_fn);
	check_permission_expression(stmt->expression, containing_fn);

	value_symbol = get_rhs_symbol(stmt->expression);
	if (value_symbol)
		check_call_symbol_permission(stmt->expression->pos, return_type, value_symbol);
}

static void check_permission_inlined_call(struct statement *stmt, struct symbol *containing_fn)
{
	check_call_symbol_permission(stmt->pos, containing_fn, stmt->inline_fn);
}

static void check_permission_compound_statement(struct statement *stmt, struct symbol *containing_fn)
{
	struct statement *s;

	FOR_EACH_PTR(stmt->stmts, s) {
		if (s)
			check_permission_statement(s, containing_fn);
	} END_FOR_EACH_PTR(s);
}

static void check_permission_switch(struct statement *stmt, struct symbol *containing_fn)
{
	check_permission_expression(stmt->switch_expression, containing_fn);
	check_permission_statement(stmt->switch_statement, containing_fn);
}

static void check_permission_iterator(struct statement *stmt, struct symbol *containing_fn)
{
	struct statement  *pre_statement = stmt->iterator_pre_statement;
	struct expression *pre_condition = stmt->iterator_pre_condition;
	struct statement  *statement = stmt->iterator_statement;
	struct statement  *post_statement = stmt->iterator_post_statement;
	struct expression *post_condition = stmt->iterator_post_condition;

	if (pre_statement)
		check_permission_statement(pre_statement, containing_fn);
	check_permission_expression(pre_condition, containing_fn);
	check_permission_statement(statement, containing_fn);
	if (post_statement)
		check_permission_statement(post_statement, containing_fn);
	check_permission_expression(post_condition, containing_fn);
}

static void check_permission_statement(struct statement *stmt, struct symbol *containing_fn)
{
	switch (stmt->type) {
		case STMT_NONE:
		case STMT_RANGE:
		case STMT_CONTEXT:
			break;

		case STMT_DECLARATION:
			check_permission_declaration(stmt, containing_fn);
			break;

		case STMT_EXPRESSION:
			check_permission_expression(stmt->expression, containing_fn);
			break;

		case STMT_ASM:
			check_permission_asm_statement(stmt, containing_fn);
			break;

		case STMT_RETURN:
			check_permission_return(stmt, containing_fn);
			break;

		case STMT_CASE:
			check_permission_statement(stmt->case_statement, containing_fn);
			break;

		case STMT_LABEL: {
			check_permission_statement(stmt->label_statement, containing_fn);
			break;
		}

		case STMT_GOTO: {
			struct expression *expr;

			if (stmt->goto_label)
				break;

			expr = stmt->goto_expression;
			if (!expr)
				break;

			/* This can happen as part of simplification */
			if (expr->type == EXPR_LABEL)
				break;

			check_permission_expression(expr, containing_fn);
			break;
		}

		case STMT_COMPOUND:
			if (stmt->inline_fn)
				check_permission_inlined_call(stmt, containing_fn);
			else
				check_permission_compound_statement(stmt, containing_fn);
			break;

		case STMT_IF:
			check_permission_expression(stmt->if_conditional, containing_fn);
			check_permission_statement(stmt->if_true, containing_fn);

			if (stmt->if_false)
				check_permission_statement(stmt->if_false, containing_fn);
			break;

		case STMT_SWITCH:
			check_permission_switch(stmt, containing_fn);
			break;

		case STMT_ITERATOR:
			check_permission_iterator(stmt, containing_fn);
			break;

		default:
			assert(0);
	}
}

static void check_permission_symbol(struct symbol *sym)
{
	struct symbol *base_type = sym->ctype.base_type;
	assert(base_type->type == SYM_FN);
	check_permission_statement(base_type->stmt, sym);
}

static void propagate_permissions(struct symbol *sym)
{
	/* for each symbol */
	struct symbol *next = sym;
	do {
		/* go through each permission */
		struct permission *permission;
		FOR_EACH_PTR(next->ctype.permissions, permission) {
			/* and propagate it to each symbol */
			struct symbol *other = sym;
			do {
				if (!has_permission(other->ctype.permissions, permission))
					add_ptr_list(&other->ctype.permissions, permission);
			} while ((other = other->same_symbol) != NULL);
		} END_FOR_EACH_PTR(permission);
	} while ((next = next->same_symbol) != NULL);
}

static void check_symbols(struct symbol_list *list)
{
	struct symbol *sym;

	FOR_EACH_PTR(list, sym) {
		expand_symbol(sym);
		propagate_permissions(sym);
	} END_FOR_EACH_PTR(sym);

	FOR_EACH_PTR(list, sym) {
		struct entrypoint *ep;

		ep = linearize_symbol(sym);
		if (ep) {
			if (dbg_entry)
				show_entry(ep);

			check_context(ep);
			check_permission_symbol(sym);
		}
	} END_FOR_EACH_PTR(sym);

	if (Wsparse_error && die_if_error)
		exit(1);
}

int main(int argc, char **argv)
{
	struct string_list *filelist = NULL;
	char *file;

	// Expand, linearize and show it.
	check_symbols(sparse_initialize(argc, argv, &filelist));
	FOR_EACH_PTR_NOTAG(filelist, file) {
		check_symbols(sparse(file));
	} END_FOR_EACH_PTR_NOTAG(file);
	return 0;
}
