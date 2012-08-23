#include "lua.hpp"
#include <lua.h>

namespace lua {

/////////////////////////////////////////////////////////////////////////////

struct do_task : public boost::static_visitor<bool>
{
    bool operator()(vm_t::tasks::call_t const& call)
    {
        boost::shared_ptr<ErlNifEnv> env(enif_alloc_env(), enif_free_env);
        std::cout << "call task: " << call.fun << std::endl;
        ERL_NIF_TERM term = enif_make_tuple2(env.get(), enif_make_atom(env.get(), "so"), enif_make_atom(env.get(), "cool"));
        enif_send(NULL, call.pid.ptr(), env.get(), term);
    }
    bool operator()(vm_t::tasks::quit_t const&)
    {
        std::cout << "quit task" << std::endl;
        return false;
    }
};

/////////////////////////////////////////////////////////////////////////////

vm_t::vm_t()
{

}

vm_t::~vm_t()
{

}

/////////////////////////////////////////////////////////////////////////////

void* vm_t::run_vm(void * vm)
{
    static_cast<vm_t*>(vm)->run();
    return 0;
}

boost::shared_ptr<vm_t> vm_t::create(ErlNifResourceType* res_type)
{
    void * buf = enif_alloc_resource(res_type, sizeof(vm_t));
    boost::shared_ptr<vm_t> result(new (buf) vm_t(), enif_release_resource);

//     result->opts_ = enif_thread_opts_create("vm_thread_opts");
    if(enif_thread_create("", &result->tid_, vm_t::run_vm, result.get(), NULL) != 0)
    {
        result.reset();
    }

    return result;
}

void vm_t::destroy(ErlNifEnv* env, void* obj)
{
    std::cout << "destroy called" << std::endl;
    static_cast<vm_t*>(obj)->stop();
}

void vm_t::run()
{
    std::cout << "run called" << std::endl;
    for(;;)
    {
        task_t task = queue_.pop();
        do_task visitor;
        if (!boost::apply_visitor(visitor, task))
        {
            break;
        }
    }
}

void vm_t::stop()
{
    std::cout << "stop called" << std::endl;
    queue_.push(tasks::quit_t());
    enif_thread_join(tid_, NULL);
    this->~vm_t();
};

void vm_t::add_task(task_t const& task)
{
    queue_.push(task);
}

}
