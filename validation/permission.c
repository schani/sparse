#include <string.h>

#define GOOD __attribute__((permission(good)))

typedef int GOOD (*good_fn_t) (int);
typedef int (*not_good_fn_t) (int);

typedef struct {
	int GOOD (*good_fn_ptr) (int);
	int (*not_good_fn_ptr) (int);
} struct_with_fn_ptr_t;

static good_fn_t good_fn_ptr;
static not_good_fn_t not_good_fn_ptr;

static struct_with_fn_ptr_t struct_with_fn_ptr;

static int doesnt_call (int x) GOOD;

extern __SIZE_TYPE__ __builtin_object_size(void *, int) GOOD;
extern char * __builtin___strcpy_chk(char *, const char *, __SIZE_TYPE__) GOOD;

static int
doesnt_call (int x)
{
	return x + 1;
}

static int GOOD
does_call (int x)
{
	return doesnt_call (x);
}

static int
good_but_not_declared (int x)
{
	return does_call (x);
}

static inline int
inline_good_but_not_declared (int x)
{
	return doesnt_call (x);
}

static int GOOD
declared_but_calls_inline_not_good (int x)
{
	return inline_good_but_not_declared (x); /* bad */
}

static int GOOD
declared_but_not_good (int x)
{
	return good_but_not_declared (x); /* bad */
}

static void GOOD
my_strcpy (char *dst, const char *src)
{
	strcpy (dst, src);
}

static int GOOD
calls_indirect (int x)
{
	return good_fn_ptr(x);
}

static int GOOD
calls_indirect_but_not_good (int x)
{
	return not_good_fn_ptr(x); /* bad */
}

static int GOOD
calls_indirect_from_struct (int x)
{
	return struct_with_fn_ptr.good_fn_ptr(x);
}

static int GOOD
calls_indirect_but_not_good_from_struct (int x)
{
	return struct_with_fn_ptr.not_good_fn_ptr(x); /* bad */
}

static int GOOD
calls_non_good_argument (not_good_fn_t fn, int x)
{
	return fn(x);		/* bad */
}

static int GOOD
calls_good_argument (good_fn_t fn, int x)
{
	return fn(x);
}

int
main (void)
{
	char bla [32];
	struct_with_fn_ptr_t *struct_ptr = &struct_with_fn_ptr;

	good_fn_ptr = doesnt_call;
	not_good_fn_ptr = doesnt_call;

	good_fn_ptr = good_but_not_declared; /* bad */
	good_fn_ptr = not_good_fn_ptr;	     /* bad */

	struct_with_fn_ptr.good_fn_ptr = good_but_not_declared; /* bad */
	struct_with_fn_ptr.not_good_fn_ptr = good_but_not_declared;

	struct_ptr->good_fn_ptr = good_but_not_declared; /* bad */
	struct_ptr->not_good_fn_ptr = good_but_not_declared;

	calls_good_argument (good_but_not_declared, 1); /* bad */

	my_strcpy (bla, "bla");
	bla [2] += declared_but_not_good (1) + declared_but_calls_inline_not_good (1);
	return bla [2] != 'e';
}
/*
 * check-name: Check permission
 *
 * check-error-start
permission.c:50:45: warning: function declared_but_calls_inline_not_good
permission.c:50:45: warning:   calls function inline_good_but_not_declared
permission.c:50:45: warning:   without permission good
permission.c:56:38: warning: function declared_but_not_good
permission.c:56:38: warning:   calls function good_but_not_declared
permission.c:56:38: warning:   without permission good
permission.c:74:31: warning: function calls_indirect_but_not_good
permission.c:74:31: warning:   calls function not_good_fn_ptr
permission.c:74:31: warning:   without permission good
permission.c:86:50: warning: function calls_indirect_but_not_good_from_struct
permission.c:86:50: warning:   calls function not_good_fn_ptr
permission.c:86:50: warning:   without permission good
permission.c:92:18: warning: function calls_non_good_argument
permission.c:92:18: warning:   calls function fn
permission.c:92:18: warning:   without permission good
permission.c:110:21: warning: function good_fn_ptr
permission.c:110:21: warning:   calls function good_but_not_declared
permission.c:110:21: warning:   without permission good
permission.c:111:21: warning: function good_fn_ptr
permission.c:111:21: warning:   calls function not_good_fn_ptr
permission.c:111:21: warning:   without permission good
permission.c:113:40: warning: function good_fn_ptr
permission.c:113:40: warning:   calls function good_but_not_declared
permission.c:113:40: warning:   without permission good
permission.c:113:40: warning: function good_fn_ptr
permission.c:113:40: warning:   calls function good_but_not_declared
permission.c:113:40: warning:   without permission good
permission.c:116:33: warning: function good_fn_ptr
permission.c:116:33: warning:   calls function good_but_not_declared
permission.c:116:33: warning:   without permission good
permission.c:116:33: warning: function good_fn_ptr
permission.c:116:33: warning:   calls function good_but_not_declared
permission.c:116:33: warning:   without permission good
permission.c:119:30: warning: function fn
permission.c:119:30: warning:   calls function good_but_not_declared
permission.c:119:30: warning:   without permission good
 * check-error-end
 */
