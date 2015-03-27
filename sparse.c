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

static int idents_equal(struct ident *i1, struct ident *i2)
{
	if (i1->len != i2->len)
		return 0;
	return !strncmp(i1->name, i2->name, i1->len);
}

static int has_permission(struct symbol *sym, struct permission *permission)
{
	struct permission *sym_permission;
	FOR_EACH_PTR(sym->ctype.permissions, sym_permission) {
		if (idents_equal(permission->name, sym_permission->name))
			return 1;
	} END_FOR_EACH_PTR(sym_permission);
	return 0;
}

static void check_call_permission(struct symbol *caller_sym, struct instruction *insn, struct permission *caller_permission)
{
	pseudo_t fn = insn->func;

	if (fn->type != PSEUDO_SYM) {
		warning(insn->pos, "function %s makes call to non-symbol function", show_ident(caller_sym->ident));
		return;
	}

	if (!has_permission(fn->sym, caller_permission)) {
		warning(insn->pos, "function %s", show_ident(caller_sym->ident));
		warning(insn->pos, "  calls function %s", show_ident(fn->sym->ident));
		warning(insn->pos, "  without permission %s", show_ident(caller_permission->name));
	}
}

static void check_permission(struct entrypoint *ep)
{
	struct symbol *sym = ep->name;
	struct permission *permission;

	FOR_EACH_PTR(sym->ctype.permissions, permission) {
		struct basic_block *bb;
		FOR_EACH_PTR(ep->bbs, bb) {
			struct instruction *insn;
			FOR_EACH_PTR(bb->insns, insn) {
				if (!insn->bb)
					continue;
				switch (insn->opcode) {
					case OP_CALL:
					case OP_INLINED_CALL:
						check_call_permission(sym, insn, permission);
						break;
					default:
						break;
				}
			} END_FOR_EACH_PTR(insn);
		} END_FOR_EACH_PTR(bb);
	} END_FOR_EACH_PTR(permission);
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
				if (!has_permission(other, permission))
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
			check_permission(ep);
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
