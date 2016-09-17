#include <string.h>
#include <inttypes.h>
#include <math.h>
#include "erl_nif.h"

#if BITNESS == 12
#define ERL_MAKE_ELEM   enif_make_uint
#define ROUND           round
#define MODULE_NAME     ecirca_small
#define MAX_SLICE       UINT16_MAX
typedef uint16_t        elem_t;
#endif
#if BITNESS == 28
#define ERL_MAKE_ELEM   enif_make_uint
#define ROUND           lround
#define MODULE_NAME     ecirca_medium
#define MAX_SLICE       UINT32_MAX
typedef uint32_t        elem_t;
#endif
#if BITNESS == 60
#define ERL_MAKE_ELEM   enif_make_uint64
/* bigger vals will be represented as bigints in erlang VM */
/* TODO: add check for this value in all functions */
#define ROUND           llround
#define MODULE_NAME     ecirca_large
#define MAX_SLICE       UINT64_MAX
typedef uint64_t        elem_t;
#endif

static const elem_t MAX_VAL = (long long) 1 << BITNESS;

#define ERL_MAKE_SIZE   enif_make_uint64
#define ERL_GET_SIZE    enif_get_uint64
#define MAX_SIZE        MAX_SLICE

#define ATOM_OK         enif_make_atom(env, "ok")
#define ATOM_ERROR      enif_make_atom(env, "error")
#define ATOM_OVERFLOW   enif_make_atom(env, "overflow")
#define BADARG          enif_make_badarg(env)
#define TUPLE2(A, B)    enif_make_tuple2(env, A, B)
#define ATOM(A)         enif_make_atom(env, A)

#define PUT_BUF(BUF, OFFSET, VAL) \
    *((__typeof__(VAL)*)(BUF + OFFSET)) = VAL; OFFSET += sizeof(VAL);
#define GET_BUF(BUF, OFFSET, VAL) \
    VAL = *((__typeof__(VAL)*)(BUF + OFFSET)); OFFSET += sizeof(VAL);

#define UNUSED          __attribute__((unused))

typedef ErlNifUInt64        length_t;
typedef double              avg_t;
typedef unsigned short int  bool_t;

static const char emptystr[] = "empty";

/* data structures */
enum ecirca_type {
    ECIRCA_LAST = 0, ECIRCA_MAX = 1, ECIRCA_MIN = 2,
    ECIRCA_AVG  = 3, ECIRCA_SUM = 4
};

enum atom_type {
    ATOM_STRONG,
    ATOM_WEAK
};

enum vtn_ret {
    VTN_OK,
    VTN_ERROR,
    VTN_ATOM
};

typedef struct {
    length_t begin;
    elem_t *circa;
    avg_t *avg;
    length_t size;
    enum ecirca_type type;
    ERL_NIF_TERM atoms[16];
    enum atom_type atom_types[16];
} ecirca_ctx;

ErlNifResourceType* ecirca_resource_t;

/* additional functions */
static int set_type(char *, enum ecirca_type *);
static length_t get_index(ecirca_ctx *, length_t);
static ERL_NIF_TERM number_to_value(ErlNifEnv *, ecirca_ctx *, length_t);
static enum vtn_ret value_to_number(ErlNifEnv *, ecirca_ctx *, ERL_NIF_TERM,
                                    elem_t *, ERL_NIF_TERM *);
static int update_value(ErlNifEnv *, ecirca_ctx *, length_t,
                        elem_t, ERL_NIF_TERM *);
static elem_t encode_atom(elem_t);
static int is_encoded_atom(elem_t);
static int is_strong_atom(ecirca_ctx *, elem_t);
static int is_weak_atom(ecirca_ctx *, elem_t);
static int is_empty(elem_t);

/* ecirca destructor */
void
ecirca_ctx_dtor(ErlNifEnv *env UNUSED, void *obj) {
    ecirca_ctx *ctx = (ecirca_ctx *) obj;

    enif_free(ctx->circa);

    if (ctx->type == ECIRCA_AVG)
        enif_free(ctx->avg);
}

/* creating resource type on load */
static int
init(ErlNifEnv *env, void **priv UNUSED, ERL_NIF_TERM info UNUSED) {
    int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    ecirca_resource_t = enif_open_resource_type(env, NULL, "circa",
                                                ecirca_ctx_dtor, flags, NULL);
    return ecirca_resource_t == NULL;
}

static ERL_NIF_TERM
new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    const ERL_NIF_TERM *pair;
    ecirca_ctx *ctx;
    ERL_NIF_TERM ret, head, tail, elem;
    length_t size;
    char typestr[5];
    enum ecirca_type type;
    unsigned i, len;
    int arity;

    if (argc != 3
        || !ERL_GET_SIZE(env, argv[0], &size)
        || size == 0
        || !enif_get_atom(env, argv[1], typestr, 5, ERL_NIF_LATIN1)
        || !set_type(typestr, &type)
        || !enif_get_list_length(env, argv[2], &len)
        || len > 14) {
        return BADARG;
    }
    if (size > MAX_SIZE) {
        return TUPLE2(ATOM_ERROR, ATOM_OVERFLOW);
    }

    ctx        = enif_alloc_resource(ecirca_resource_t, sizeof(ecirca_ctx));
    ctx->begin = 0;
    ctx->circa = enif_alloc(sizeof(elem_t) * size);
    memset(ctx->circa, 0xFF, sizeof(elem_t) * size);
    ctx->size  = size;
    ctx->type  = type;

    ctx->atoms[15]      = enif_make_atom(env, "empty");
    ctx->atom_types[15] = ATOM_WEAK;
    ctx->atoms[0]      = enif_make_atom(env, "none");
    ctx->atom_types[0] = ATOM_WEAK;
    i = 1;
    head = argv[2];
    while (enif_get_list_cell(env, head, &elem, &tail)) {
        if (!enif_get_tuple(env, elem, &arity, &pair)
            || arity != 2
            || !enif_is_atom(env, pair[0])
            || !enif_is_atom(env, pair[1])) {
            return BADARG;
        }
        if (!enif_compare(ATOM("weak"), pair[1])) {
            ctx->atom_types[i] = ATOM_WEAK;
        } else if (!enif_compare(ATOM("strong"), pair[1])) {
            ctx->atom_types[i] = ATOM_STRONG;
        } else {
            return BADARG;
        }
        ctx->atoms[i] = pair[0];
        i++;
        head = tail;
    }
    /* fill the rest with "none" */
    for (; i < 15; i++) {
        ctx->atoms[i] = enif_make_atom(env, "none");
        ctx->atom_types[i] = ATOM_WEAK;
    }

    if (type == ECIRCA_AVG) {
        ctx->avg = enif_alloc(sizeof(avg_t) * size);
        memset(ctx->avg, 0x00, sizeof(avg_t) * size);
    } else {
        ctx->avg = NULL;
    }

    ret = enif_make_resource(env, ctx);
    enif_release_resource(ctx);

    return ret;
}

static ERL_NIF_TERM
push(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ecirca_ctx *ctx;
    elem_t val;
    length_t idx;
    ERL_NIF_TERM ret;

    if (argc != 2) {
        return BADARG;
    }
    if (!enif_get_resource(env, argv[0], ecirca_resource_t, (void **) &ctx)) {
        return BADARG;
    }

    if (++ctx->begin >= ctx->size + 1) {
        ctx->begin = 1;
    }
    idx = ctx->begin - 1;

    switch (value_to_number(env, ctx, argv[1], &val, &ret)) {
        case VTN_ERROR: return ret;
        case VTN_ATOM:
            val = encode_atom(val);
            if (ctx->type == ECIRCA_AVG) {
                ctx->avg[idx] = 0;
            }
            ctx->circa[idx] = val;
            break;
        case VTN_OK:
            if (ctx->type == ECIRCA_AVG) {
                ctx->avg[idx] = (avg_t) val;
                ctx->circa[idx] = 1;
            } else {
                ctx->circa[idx] = val;
            }
            break;
    }

    return ATOM_OK;
}

static ERL_NIF_TERM
get(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ecirca_ctx *ctx;
    length_t i, idx;
    ERL_NIF_TERM ret;

    if (argc != 2
        || !ERL_GET_SIZE(env, argv[1], &i)
        || !enif_get_resource(env, argv[0], ecirca_resource_t, (void **) &ctx)
        || i > ctx->size || i == 0) {
        return BADARG;
    }

    idx = get_index(ctx, i);
    ret = number_to_value(env, ctx, idx);

    return TUPLE2(ATOM_OK, ret);
}

static ERL_NIF_TERM
set(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ecirca_ctx *ctx;
    length_t i, idx;
    elem_t val;
    ERL_NIF_TERM ret, old_term, new_term;

    if (argc != 3
        || !enif_get_resource(env, argv[0], ecirca_resource_t, (void **) &ctx)
        || !ERL_GET_SIZE(env, argv[1], &i)
        || i > ctx->size || i == 0) {
        return BADARG;
    }

    idx = get_index(ctx, i);

    old_term = number_to_value(env, ctx, idx);

    switch (value_to_number(env, ctx, argv[2], &val, &ret)) {
        case VTN_ERROR: return ret;
        case VTN_ATOM:
            if (ctx->type == ECIRCA_AVG) {
                ctx->avg[idx] = 0;
            }
            ctx->circa[idx] = encode_atom(val);
            break;
        default:
          if (ctx->type == ECIRCA_AVG) {
              ctx->circa[idx] = 1;
              ctx->avg[idx] = (avg_t) val;
          } else {
              ctx->circa[idx] = val;
          }
    }

    new_term = number_to_value(env, ctx, idx);

    return TUPLE2(ATOM_OK, TUPLE2(old_term, new_term));
}

static ERL_NIF_TERM
update(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ecirca_ctx *ctx;
    length_t i, idx;
    elem_t val;
    ERL_NIF_TERM ret, old_term, new_term;

    if (argc != 3
        || !enif_get_resource(env, argv[0], ecirca_resource_t, (void **) &ctx)
        || !ERL_GET_SIZE(env, argv[1], &i)
        || i > ctx->size || i == 0) {
        return BADARG;
    }

    idx = get_index(ctx, i);

    old_term = number_to_value(env, ctx, idx);

    switch (value_to_number(env, ctx, argv[2], &val, &ret)) {
        case VTN_ERROR: return ret;
        case VTN_ATOM:
            if (is_empty(ctx->circa[idx]) ||
                ctx->atom_types[val] == ATOM_STRONG) {
                ctx->circa[idx] = encode_atom(val);
                if (ctx->type == ECIRCA_AVG) {
                    ctx->avg[idx] = 0;
                }
            } /* weak atoms can't override existing value */
            break;
        case VTN_OK:
            if (!update_value(env, ctx, idx, val, &ret)) {
                return ret;
            }
            break;
    }

    new_term = number_to_value(env, ctx, idx);

    return TUPLE2(ATOM_OK, TUPLE2(old_term, new_term));
}

static ERL_NIF_TERM
slice(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ecirca_ctx *ctx;
    length_t start, end, slicesize, idx, i, a;
    ERL_NIF_TERM *terms;
    int incr;

    if (argc != 3
        || !enif_get_resource(env, argv[0], ecirca_resource_t, (void **) &ctx)
        || !ERL_GET_SIZE(env, argv[1], &start)
        || !ERL_GET_SIZE(env, argv[2], &end)
        || start > ctx->size || start == 0
        || end > ctx->size || end == 0) {
        return BADARG;
    }

    if (start > end) {
        incr = -1;
        slicesize = start - end + 1;
    } else {
        incr = 1;
        slicesize = end - start + 1;
    }

    if (slicesize > MAX_SLICE) {
        return TUPLE2(ATOM_ERROR, ATOM("slice_too_big"));
    }

    /* create slice */
    terms = enif_alloc(sizeof(ERL_NIF_TERM) * slicesize);

    for (a = 0, i = start; i != end + incr; i += incr) {
        idx = get_index(ctx, i);
        terms[a++] = number_to_value(env, ctx, idx);
    }

    return TUPLE2(ATOM_OK, enif_make_list_from_array(env, terms, slicesize));
}

/* getter function for size */
static ERL_NIF_TERM
size(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ecirca_ctx *ctx;

    if (argc != 1) {
        return enif_make_badarg(env);
    }
    if (!enif_get_resource(env, argv[0], ecirca_resource_t, (void **) &ctx)) {
        return enif_make_badarg(env);
    }

    return TUPLE2(ATOM_OK, ERL_MAKE_SIZE(env, ctx->size));
}

/* getter functions for constants */
static ERL_NIF_TERM
max_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[] UNUSED) {
    if (argc != 0) {
        return BADARG;
    }

    return TUPLE2(ATOM_OK, ERL_MAKE_SIZE(env, MAX_SIZE));
}

static ERL_NIF_TERM
max_slice(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[] UNUSED) {
    if (argc != 0) {
        return BADARG;
    }

    return TUPLE2(ATOM_OK, ERL_MAKE_SIZE(env, MAX_SLICE));
}

static ERL_NIF_TERM
save(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ecirca_ctx *ctx;
    ERL_NIF_TERM ret;
    unsigned char *bin_data;
    uint64_t buflen, headerlen, atomslen, elemslen, avglen, i, offset;
    unsigned int atomlens[16];

    if (argc != 1
        || !enif_get_resource(env, argv[0], ecirca_resource_t,
                              (void **) &ctx))
        return BADARG;

    for (i = 0; i < 16; i++) {
        enif_get_atom_length(env, ctx->atoms[i],
                             &(atomlens[i]), ERL_NIF_LATIN1);
        atomlens[i]++; /* null-ending should have place */
    }

    headerlen = (sizeof(length_t) +              /* ctx->size       */
                 sizeof(length_t) +              /* ctx->begin      */
                 sizeof(enum ecirca_type) +      /* ctx->type       */
                 sizeof(unsigned int) +          /* bitness         */
                 (sizeof(enum atom_type) * 16)); /* ctx->atom_types */

    atomslen = 0;
    for (i = 0; i < 16; i++) {
        atomslen += atomlens[i];
    }

    elemslen = sizeof(elem_t) * ctx->size;

    if (ctx->type == ECIRCA_AVG) {
        avglen = sizeof(avg_t) * ctx->size;
    } else {
        avglen = 0;
    }

    buflen = (headerlen +
              atomslen +
              elemslen +
              avglen);

    bin_data = enif_make_new_binary(env, buflen, &ret);

    /* format is size-begin-type-value_type-atom_types-atoms-circa-[count]*/
    memset(bin_data, 0x00, headerlen);

    offset = 0;
    PUT_BUF(bin_data, offset, ctx->size);
    PUT_BUF(bin_data, offset, ctx->begin);
    PUT_BUF(bin_data, offset, ctx->type);
    PUT_BUF(bin_data, offset, BITNESS);
    for (i = 0; i < 16; i++) {
        PUT_BUF(bin_data, offset, ctx->atom_types[i]);
    }

    for (i = 0; i < 16; i++) {
        enif_get_atom(env, ctx->atoms[i], (char*)(bin_data + offset),
                      atomlens[i], ERL_NIF_LATIN1);
        offset += atomlens[i];
    }

    memcpy(bin_data + offset, ctx->circa, elemslen);

    if (ctx->type == ECIRCA_AVG) {
        offset += elemslen;
        memcpy(bin_data + offset, ctx->avg, avglen);
    }

    return TUPLE2(ATOM_OK, ret);
}

static ERL_NIF_TERM
load(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ecirca_ctx *ctx;
    ErlNifBinary bin;
    ERL_NIF_TERM ret;
    uint64_t buflen, headerlen, i, offset, atomslen;
    unsigned value_type;

    if (argc != 1) {
        return BADARG;
    }
    if (!enif_is_binary(env, argv[0])) {
        return BADARG;
    }
    if (!enif_inspect_binary(env, argv[0], &bin)) {
        return BADARG;
    };

    headerlen = (sizeof(length_t) +
                 sizeof(length_t) +
                 sizeof(enum ecirca_type) +
                 sizeof(unsigned int) +
                 (sizeof(enum atom_type) * 16));

    if (bin.size < headerlen) {
        return TUPLE2(ATOM_ERROR, ATOM("bad_binary_size_head"));
    }

    /* format is size-begin-type-circa-[count]*/
    offset = 0;
    ctx = enif_alloc_resource(ecirca_resource_t, sizeof(ecirca_ctx));
    GET_BUF(bin.data, offset, ctx->size);
    GET_BUF(bin.data, offset, ctx->begin);
    GET_BUF(bin.data, offset, ctx->type);
    GET_BUF(bin.data, offset, value_type);
    for (i = 0; i < 16; i++) {
        GET_BUF(bin.data, offset, ctx->atom_types[i]);
    }

    if (value_type != BITNESS) {
        return TUPLE2(ATOM_ERROR, ATOM("wrong_ecirca_value_type"));
    }

    if (ctx->size > MAX_SIZE) {
        return TUPLE2(ATOM_ERROR, ATOM("max_size"));
    }
    if (ctx->begin > ctx->size) {
        return TUPLE2(ATOM_ERROR, ATOM("bad_ecirca_begin"));
    }

    if (ctx->type > ECIRCA_SUM) {
        return TUPLE2(ATOM_ERROR, ATOM("bad_ecirca_type"));
    }

    atomslen = offset;
    for (i = 0; i < 16; i++) {
        ctx->atoms[i] = enif_make_atom(env, (const char*)(bin.data + offset));
        offset += strlen((const char*)(bin.data + offset)) + 1;
    }
    atomslen = offset - atomslen;

    buflen = (headerlen + atomslen + sizeof(elem_t) * ctx->size);
    if (ctx->type == ECIRCA_AVG) {
        buflen += (sizeof(avg_t) * ctx->size);
    }
    if (bin.size != buflen) {
        return TUPLE2(ATOM_ERROR, ATOM("bad_binary_size_body"));
    }

    ctx->circa = enif_alloc(sizeof(elem_t) * ctx->size);
    memcpy(ctx->circa, bin.data + offset, ctx->size * sizeof(elem_t));
    if (ctx->type == ECIRCA_AVG) {
        offset += (ctx->size * sizeof(elem_t));
        ctx->avg = enif_alloc(ctx->size * sizeof(avg_t));
        memcpy(ctx->avg, bin.data + offset, ctx->size * sizeof(avg_t));
    }

    ret = enif_make_resource(env, ctx);
    enif_release_resource(ctx);

    return ret;
}

/* for setting ecirca type */
static int
set_type(char *str, enum ecirca_type *type) {
    if (strcmp(str, "max") == 0) {
        *type = ECIRCA_MAX; return 1;
    }
    else if (strcmp(str, "min") == 0) {
        *type = ECIRCA_MIN; return 1;
    }
    else if (strcmp(str, "avg") == 0) {
        *type = ECIRCA_AVG; return 1;
    }
    else if (strcmp(str, "sum") == 0) {
        *type = ECIRCA_SUM; return 1;
    }
    else if (strcmp(str, "last") == 0) {
        *type = ECIRCA_LAST; return 1;
    }
    return 0;
}

/* get array index with respect to array bounds */
static length_t
get_index(ecirca_ctx *ctx, length_t i) {
    if (i > ctx->begin) {
        return ctx->size + ctx->begin - i;
    } else {
        return ctx->begin - i;
    }
}

static ERL_NIF_TERM
number_to_value(ErlNifEnv *env, ecirca_ctx *ctx, length_t idx) {
    int atom_idx;

    atom_idx = (int)(ctx->circa[idx] >> BITNESS);
    if (atom_idx > 0 && atom_idx < 16) {
        return ctx->atoms[atom_idx];
    }
    if (ctx->type == ECIRCA_AVG) {
        return ERL_MAKE_ELEM(env, (elem_t)ROUND(ctx->avg[idx]));
    }
    return ERL_MAKE_ELEM(env, ctx->circa[idx]);
}

/* returns 0 if error occured and caller should return "ret";
   returns 1 if value is a number and caller should use "num" as a number;
   returns 2 if value is an atom and caller should use "num" as an index in
             atoms table */
static enum vtn_ret
value_to_number(ErlNifEnv *env, ecirca_ctx *ctx,
                ERL_NIF_TERM val, elem_t *num, ERL_NIF_TERM *ret) {
    int i;
    ErlNifUInt64 val_big;

    if (!enif_get_uint64(env, val, &val_big)) {
        if (!enif_is_atom(env, val)) {
            *ret = BADARG;
            return VTN_ERROR;
        }
        if (!enif_compare(val, ATOM("empty"))) {
            *num = 15;
            return VTN_ATOM;
        }
        for (i = 1; i < 15; i++) {
            if (!enif_compare(val, ctx->atoms[i])) {
                *num = i;
                return VTN_ATOM;
            }
        }
        *ret = TUPLE2(ATOM_ERROR, ATOM("unknown_atom"));
        return VTN_ERROR;
    }

    if (val_big >= MAX_VAL) {
        *ret = TUPLE2(ATOM_ERROR, ATOM_OVERFLOW);
        return VTN_ERROR;
    }

    *num = (elem_t) val_big;
    return VTN_OK;
}

static int
update_value(ErlNifEnv *env, ecirca_ctx *ctx,
             length_t idx, elem_t val, ERL_NIF_TERM *ret) {
    elem_t sum;

    if (is_strong_atom(ctx, ctx->circa[idx])) {
        return 1;
    }

    if (ctx->type == ECIRCA_AVG) {
        if (is_encoded_atom(ctx->circa[idx])) {
            ctx->avg[idx] = (avg_t) val;
            ctx->circa[idx] = 1;
            return 1;
        }
        if (ctx->circa[idx] == (MAX_VAL - 1)) {
            *ret = TUPLE2(ATOM_ERROR, ATOM_OVERFLOW);
            return 0;
        }
        /* moving cumulative average */
        /* Kahan summation can be used */
        ctx->circa[idx]++;
        ctx->avg[idx] += ((val - ctx->avg[idx]) / ctx->circa[idx]);
        return 1;
    }
    if (is_weak_atom(ctx, ctx->circa[idx])) {
        ctx->circa[idx] = val;
        return 1;
    }
    switch (ctx->type) {
        case ECIRCA_MAX:
            if (val > ctx->circa[idx]) {
                ctx->circa[idx] = val;
            }
            break;
        case ECIRCA_MIN:
            if (val < ctx->circa[idx]) {
                ctx->circa[idx] = val;
            }
            break;
        case ECIRCA_SUM:
            sum = val + ctx->circa[idx];
            if (sum >= val && sum >= ctx->circa[idx] && sum < MAX_VAL) { /* no overflow */
                ctx->circa[idx] = sum;
            } else {
                *ret = TUPLE2(ATOM_ERROR, ATOM_OVERFLOW);
                return 0;
            }
            break;
        case ECIRCA_LAST:
            ctx->circa[idx] = val;
            break;
        default: break;
    }
    return 1;
}

static elem_t
encode_atom(elem_t val) {
    return (elem_t) (val << BITNESS);
}

static int
is_encoded_atom(elem_t val) {
    return ((int) (val >> BITNESS)) > 0;
}

static int
is_strong_atom(ecirca_ctx *ctx, elem_t val) {
    return (is_encoded_atom(val) &&
            (ctx->atom_types[(int) (val >> BITNESS)]) == ATOM_STRONG);
}

static int
is_weak_atom(ecirca_ctx *ctx, elem_t val) {
    return (is_encoded_atom(val) &&
            (ctx->atom_types[(int) (val >> BITNESS)]) == ATOM_WEAK);
}

static int
is_empty(elem_t val) {
    return ((int) (val >> BITNESS)) == 15;
}

static ErlNifFunc funcs[] =
{
    {"new",       3, new,       0},
    {"push",      2, push,      0},
    {"get",       2, get,       0},
    {"set",       3, set,       0},
    {"update",    3, update,    0},
    {"slice",     3, slice,     0},
    /* getter functions */
    {"size",      1, size,      0},
    {"max_size",  0, max_size,  0},
    {"max_slice", 0, max_slice, 0},
    {"save",      1, save,      0},
    {"load",      1, load,      0}
};


#define ERL_NIF_INIT_WRAPPER(name)                              \
    ERL_NIF_INIT(name, funcs, &init, NULL, NULL, NULL)

/* Unfortunately, 'erl_nif.h' doesn't expect marcos in place of 'name'
   argument, so we're forced to make a wrapper, which will expand 'name'
   value before calling 'ERL_NIF_INIT'. */
ERL_NIF_INIT_WRAPPER(MODULE_NAME)

#undef ERL_NIF_INIT_WRAPPER
