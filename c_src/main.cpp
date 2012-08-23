#include "lua.hpp"
#include "types.hpp"
#include "utils.hpp"
#include <algorithm>


using namespace erlcpp;

/////////////////////////////////////////////////////////////////////////////

static struct {
    ERL_NIF_TERM ok;
    ERL_NIF_TERM error;
    ERL_NIF_TERM enomem;
    ERL_NIF_TERM invalid_args;
    ERL_NIF_TERM invalid_type;
    ERL_NIF_TERM not_implemented;
} atoms;

/////////////////////////////////////////////////////////////////////////////

static ErlNifResourceType * res_type = 0;

static int load(ErlNifEnv *env, void **priv, ERL_NIF_TERM load_info)
{
    atoms.ok                = enif_make_atom(env, "ok");
    atoms.error             = enif_make_atom(env, "error");
    atoms.enomem            = enif_make_atom(env, "enomem");
    atoms.invalid_args      = enif_make_atom(env, "invalid_args");
    atoms.invalid_type      = enif_make_atom(env, "invalid_type");
    atoms.not_implemented   = enif_make_atom(env, "not_implemented");

    res_type = enif_open_resource_type(
        env, "lua", "lua_vm", lua::vm_t::destroy,
        static_cast<ErlNifResourceFlags>(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER), NULL
    );

    return (!!res_type) ? 0 : -1;
}

/////////////////////////////////////////////////////////////////////////////

static ERL_NIF_TERM open(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    boost::shared_ptr<lua::vm_t> vm = lua::vm_t::create(res_type);
    ERL_NIF_TERM result = enif_make_resource(env, vm.get());
    return enif_make_tuple2(env, atoms.ok, result);
}

static ERL_NIF_TERM close(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 1)
    {
        return enif_make_tuple2(env, atoms.error, atoms.invalid_args);
    }

    lua::vm_t * vm = NULL;
    if(!enif_get_resource(env, argv[0], res_type, reinterpret_cast<void**>(&vm))) {
        return enif_make_badarg(env);
    }

    vm->stop();
    enif_release_resource(vm);
    return atoms.ok;
}

static ERL_NIF_TERM call(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try
    {
        if (argc != 2)
        {
            return enif_make_tuple2(env, atoms.error, atoms.invalid_args);
        }

        lua::vm_t * vm = NULL;
        if(!enif_get_resource(env, argv[0], res_type, reinterpret_cast<void**>(&vm))) {
            return enif_make_badarg(env);
        }

        erlcpp::tuple_t tuple = erlcpp::get_tuple(env, argv[1]);

        lpid_t pid = boost::get<lpid_t>(tuple.at(0));
        atom_t fun = boost::get<atom_t>(tuple.at(1));
        list_t args = boost::get<list_t>(tuple.at(2));
        lua::vm_t::tasks::call_t call(pid, fun, args);
        vm->add_task(lua::vm_t::task_t(call));

        return enif_make_tuple2(env, atoms.ok, enif_make_atom(env, "cool"));
    }
    catch( std::exception & ex )
    {
        return enif_make_tuple2(env, atoms.error, enif_make_atom(env, ex.what()));
    }
}

/////////////////////////////////////////////////////////////////////////////

static ErlNifFunc nif_funcs[] = {
    {"open", 0, open},
    {"call", 2, call},
    {"close", 1, close}
};

ERL_NIF_INIT(moon, nif_funcs, &load, NULL, NULL, NULL)

/////////////////////////////////////////////////////////////////////////////

