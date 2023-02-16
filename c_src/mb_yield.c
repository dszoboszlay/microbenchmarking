#include <erl_nif.h>
#include <sched.h>

static ERL_NIF_TERM os_yield(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    sched_yield();
    return enif_make_list(env, 0);
}

static ErlNifFunc nif_funcs[] =
{
    {"os_yield", 0, os_yield}
};

ERL_NIF_INIT(mb_runner,nif_funcs,NULL,NULL,NULL,NULL)