#include "erl_nif.h"
#include <stdio.h>

static ErlNifResourceType* test_RESOURCE = NULL;

// Prototypes
static ERL_NIF_TERM get_bin_address(ErlNifEnv* env, int argc,
                                          const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] =
{
    {"get_bin_address", 1, get_bin_address}
};

static ERL_NIF_TERM get_bin_address(ErlNifEnv* env, int argc,
                                          const ERL_NIF_TERM argv[])
{
    ErlNifFunc func;
    // enif_inspect_binary(env, argv[0], &func);
    char buf[256];
    sprintf(buf, "func: name=%p, fptr=%p", func.name, func.fptr);
    // sprintf(buf, "func: name=%p, arity=%d, fptr=%p, flags=%d", func.name, func.arity, func.fptr, func.flags);
//    sprintf(buf, "bin: size=%zu, ptr=%p", bin.size, bin.data);
    return enif_make_string(env, buf, ERL_NIF_LATIN1);
}

static void test_resource_cleanup(ErlNifEnv* env, void* arg)
{
    /* Delete any dynamically allocated memory stored in test_handle */
    /* test_handle* handle = (test_handle*)arg; */
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ErlNifResourceFlags flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    ErlNifResourceType* rt = enif_open_resource_type(env, NULL,
                                                     "test_resource",
                                                     &test_resource_cleanup,
                                                     flags, NULL);
    if (rt == NULL)
        return -1;

    test_RESOURCE = rt;

    return 0;
}

ERL_NIF_INIT(binary_tools, nif_funcs, &on_load, NULL, NULL, NULL);
