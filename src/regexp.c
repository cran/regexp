#include "Rinternals.h"
#include <sys/types.h>
#include <regex.h>
static SEXP R_AllocatePtr(size_t nmemb, size_t size, SEXP tag)
{
    SEXP data, val;
    int bytes;
    if (INT_MAX / size < nmemb)
        error("allocation request is too large");
    bytes = nmemb * size;
    PROTECT(data = allocString(bytes));
    memset(CHAR(data), 0, bytes);
    val = R_MakeExternalPtr(CHAR(data), tag, data);
    UNPROTECT(1);
    return val;
}
static int sexp2int(SEXP s, Rboolean na_ok)
{
    int val = NA_INTEGER;
    switch (TYPEOF(s)) {
    case LGLSXP:
        if (LENGTH(s) == 1 && LOGICAL(s)[0] != NA_LOGICAL)
            val = LOGICAL(s)[0] != 0;
        break;
    case INTSXP:
        if (LENGTH(s) == 1)
            val = INTEGER(s)[0];
        break;
    case REALSXP:
        if (LENGTH(s) == 1 && R_FINITE(REAL(s)[0]))
            val = (int) (REAL(s)[0]);
        break;
    case CPLXSXP:
        if (LENGTH(s) == 1 &&
            R_FINITE(COMPLEX(s)[0].r) && R_FINITE(COMPLEX(s)[0].i) &&
            COMPLEX(s)[0].i == 0.0)
            val = (int) (COMPLEX(s)[0].r);
        break;
    }
    if (! na_ok && val == NA_INTEGER)
        error("not a valid integer");
    return val;
}
static char *sexp2char_p(SEXP s)
{
    if (TYPEOF(s) != STRSXP || length(s) != 1)
        error("argument not a string vector of length one");
    return CHAR(STRING_ELT(s, 0));
}
static void *sexp2ptr(SEXP s, Rboolean null_ok, SEXP tag, char *type)
{
    void *p;
    if (TYPEOF(s) != EXTPTRSXP || R_ExternalPtrTag(s) != tag)
        error("bad %s pointer", type);
    p = R_ExternalPtrAddr(s);
    if (! null_ok && p == NULL)
        error("null %s pointer", type);
    return p;
}
static void defineIntVar(SEXP env, char *name, int ival)
{
    SEXP sym, val;
    sym = install(name);
    PROTECT(val = ScalarInteger(ival));
    defineVar(sym, val, env);
    UNPROTECT(1);
}
static SEXP REGEX_type_tag;
SEXP REGEXP_make_regex_t(SEXP rn)
{
    int n = sexp2int(rn, FALSE);
    if (n <= 0) return R_NilValue;
    else return R_AllocatePtr(n, sizeof(regex_t), REGEX_type_tag);
}
static regex_t *sexp2regex_t_p(SEXP s, Rboolean null_ok)
{
    return sexp2ptr(s, null_ok, REGEX_type_tag, "regex_t");
}
SEXP REGEXP_regex_t_re_nsub(SEXP rp, SEXP ri)
{
  regex_t *p = sexp2regex_t_p(rp, FALSE);
  return ScalarInteger(p[sexp2int(ri, FALSE)].re_nsub);
}
static SEXP REGMATCH_type_tag;
SEXP REGEXP_make_regmatch_t(SEXP rn)
{
    int n = sexp2int(rn, FALSE);
    if (n <= 0) return R_NilValue;
    else return R_AllocatePtr(n, sizeof(regmatch_t), REGMATCH_type_tag);
}
static regmatch_t *sexp2regmatch_t_p(SEXP s, Rboolean null_ok)
{
    return sexp2ptr(s, null_ok, REGMATCH_type_tag, "regmatch_t");
}
SEXP REGEXP_regmatch_t_rm_so(SEXP rp, SEXP ri)
{
  regmatch_t *p = sexp2regmatch_t_p(rp, FALSE);
  return ScalarInteger(p[sexp2int(ri, FALSE)].rm_so);
}

SEXP REGEXP_regmatch_t_rm_eo(SEXP rp, SEXP ri)
{
  regmatch_t *p = sexp2regmatch_t_p(rp, FALSE);
  return ScalarInteger(p[sexp2int(ri, FALSE)].rm_eo);
}
static void finalize_regexp(SEXP s)
{
    regex_t *re = sexp2regex_t_p(s, TRUE);
    if (re != NULL)
        regfree(re);
}

SEXP REGEXP_register(SEXP rre)
{
    sexp2regex_t_p(rre, FALSE);
    R_RegisterCFinalizer(rre, finalize_regexp);
    return R_NilValue;
}
SEXP REGEXP_regerror(SEXP rcode, SEXP rre)
{
    char buf[512];
    int len;
    strcpy(buf, "regex error: ");
    len = strlen(buf);
    regerror(sexp2int(rcode, FALSE), sexp2regex_t_p(rre, FALSE),
             buf + len, sizeof(buf) - len);
    error(buf);
    return R_NilValue; /* not reached */
}
SEXP REGEXP_regcomp(SEXP rre, SEXP pat, SEXP rflags)
{
    return ScalarInteger(regcomp(sexp2regex_t_p(rre, FALSE),
                                 sexp2char_p(pat),
                                 sexp2int(rflags, FALSE)));
}
SEXP REGEXP_regexec(SEXP rre, SEXP str, SEXP rn, SEXP rrm, SEXP rflags)
{
    return ScalarInteger(regexec(sexp2regex_t_p(rre, FALSE),
                                 sexp2char_p(str),
                                 sexp2int(rn, FALSE),
                                 sexp2regmatch_t_p(rrm, FALSE),
                                 sexp2int(rflags, FALSE)));
}
SEXP REGEXP_deHex(SEXP s)
{
    char out[2], *str = sexp2char_p(s);
    int ch;
    if (sscanf(str, "%x", &ch) <= 0)
        error("bad hex string");
    out[0] = ch;
    out[1] = 0;
    return ScalarString(mkChar(out));
}
SEXP REGEXP_init(SEXP env)
{
    REGEX_type_tag = install("REGEX_TYPE_TAG");
    REGMATCH_type_tag = install("REGMATCH_TYPE_TAG");

    defineIntVar(env, "REG.EXTENDED", REG_EXTENDED);
    defineIntVar(env, "REG.ICASE", REG_ICASE);
    defineIntVar(env, "REG.NEWLINE", REG_NEWLINE);
    defineIntVar(env, "REG.NOSUB", REG_NOSUB);
    defineIntVar(env, "REG.NOMATCH", REG_NOMATCH);

    defineIntVar(env, "REG.NOTBOL", REG_NOTBOL);
    defineIntVar(env, "REG.NOTEOL", REG_NOTEOL);
    return R_NilValue;
}
