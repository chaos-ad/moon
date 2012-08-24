#include "lua.hpp"

 // I'm ugly and I know it:
extern "C"
{
    #include <lua.h>
    #include <lualib.h>
    #include <lauxlib.h>
}

#include <cstdio>


namespace lua {

/////////////////////////////////////////////////////////////////////////////

struct perform_task : public boost::static_visitor<bool>
{
    perform_task(vm_t & vm) : vm_(vm) {};

    bool operator()(vm_t::tasks::call_t const& call)
    {
        enif_fprintf(stderr, "*** call task: ~s\n", call.fun.c_str());
        boost::shared_ptr<ErlNifEnv> env(enif_alloc_env(), enif_free_env);
        ERL_NIF_TERM term = enif_make_tuple2(env.get(), enif_make_atom(env.get(), "so"), enif_make_atom(env.get(), "cool"));
        enif_send(NULL, vm_.erl_pid().ptr(), env.get(), term);
        return true;
    }
    bool operator()(vm_t::tasks::resp_t const& resp)
    {
        enif_fprintf(stderr, "*** resp task: ~s\n");
        return true;
    }
    bool operator()(vm_t::tasks::quit_t const&)
    {
        enif_fprintf(stderr, "*** quit task\n");
        return false;
    }
private :
    vm_t & vm_;
};

/////////////////////////////////////////////////////////////////////////////

vm_t::vm_t(erlcpp::lpid_t const& pid)
    : pid_(pid)
    , luastate_(::luaL_newstate())
{
    luaL_openlibs(luastate_),
    enif_fprintf(stderr, "*** construct the vm\n");
}

vm_t::~vm_t()
{
//     lua_close(luastate_);
    enif_fprintf(stderr, "*** destruct the vm\n");
}

/////////////////////////////////////////////////////////////////////////////

boost::shared_ptr<vm_t> vm_t::create(ErlNifResourceType* res_type, erlcpp::lpid_t const& pid)
{
    void * buf = enif_alloc_resource(res_type, sizeof(vm_t));
    // TODO: may leak, need to guard agaist
    boost::shared_ptr<vm_t> result(new (buf) vm_t(pid), enif_release_resource);

    if(enif_thread_create(NULL, &result->tid_, vm_t::thread_run, result.get(), NULL) != 0) {
        result.reset();
    }

    return result;
}

void vm_t::destroy(ErlNifEnv* env, void* obj)
{
    static_cast<vm_t*>(obj)->stop();
    static_cast<vm_t*>(obj)->~vm_t();
}

void vm_t::run()
{
    for(;;)
    {
        task_t task = queue_.pop();
        perform_task visitor(*this);
        if (!boost::apply_visitor(visitor, task))
        {
            break;
        }
    }
}

void vm_t::stop()
{
    queue_.push(tasks::quit_t());
    enif_thread_join(tid_, NULL);
};

void vm_t::add_task(task_t const& task)
{
    queue_.push(task);
}

void* vm_t::thread_run(void * vm)
{
    static_cast<vm_t*>(vm)->run();
    return 0;
}

}