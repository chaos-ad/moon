#include "lua.hpp"
#include "utils.hpp"
#include "errors.hpp"

namespace lua {

/////////////////////////////////////////////////////////////////////////////

class quit_tag {};

template <class worker_t>
typename worker_t::result_type perform_task(vm_t & vm)
{
    vm_t::task_t task = vm.get_task();
    worker_t worker(vm);
    return boost::apply_visitor(worker, task);
}

class result_handler : public boost::static_visitor<erlcpp::term_t>
{
public :
    result_handler(vm_t & vm) : vm_(vm) {};
    erlcpp::term_t operator()(vm_t::tasks::call_t const&) { throw errors::unexpected_msg(); }
    erlcpp::term_t operator()(vm_t::tasks::quit_t const&) { throw errors::unexpected_msg(); }
    erlcpp::term_t operator()(vm_t::tasks::resp_t const& resp) { return resp.term; }

private :
    vm_t & vm_;
};

struct call_handler : public boost::static_visitor<void>
{
    call_handler(vm_t & vm) : vm_(vm) {};

    void operator()(vm_t::tasks::call_t const& call)
    {
        enif_fprintf(stderr, "*** call task: %s\n", call.fun.c_str());

        erlcpp::tuple_t callback(2);

        erlcpp::list_t args;
        erlcpp::tuple_t arg(2);
        arg[0] = erlcpp::atom_t("hello");
        arg[1] = erlcpp::num_t(100);
        args.push_back(arg);
        erlcpp::tuple_t mfa(3);
        mfa[0] = erlcpp::atom_t("moon");
        mfa[1] = erlcpp::atom_t("test");
        mfa[2] = args;

        callback[0] = erlcpp::atom_t("moon_callback");
        callback[1] = mfa;

        {
            boost::shared_ptr<ErlNifEnv> env(enif_alloc_env(), enif_free_env);
            enif_send(NULL, vm_.erl_pid().ptr(), env.get(), erlcpp::to_erl(env.get(), callback));
        }

        erlcpp::term_t result = perform_task<result_handler>(vm_);

        erlcpp::tuple_t response(2);
        response[0] = erlcpp::atom_t("moon_response");
        response[1] = result;

        {
            boost::shared_ptr<ErlNifEnv> env(enif_alloc_env(), enif_free_env);
            enif_send(NULL, vm_.erl_pid().ptr(), env.get(), erlcpp::to_erl(env.get(), response));
        }
    }
    void operator()(vm_t::tasks::resp_t const& resp)
    {
        enif_fprintf(stderr, "*** resp task: ~s\n");
    }
    void operator()(vm_t::tasks::quit_t const&)
    {
        enif_fprintf(stderr, "*** quit task\n");
        throw quit_tag();
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
    lua_close(luastate_);
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
    try
    {
        for(;;)
        {
            perform_task<call_handler>(*this);
        }
    }
    catch(quit_tag) {}
    catch(std::exception & ex)
    {
        enif_fprintf(stderr, "*** exception in vm thread: %s\n", ex.what());
    }
    catch(...) {}
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

vm_t::task_t vm_t::get_task()
{
    return queue_.pop();
}


void* vm_t::thread_run(void * vm)
{
    static_cast<vm_t*>(vm)->run();
    return 0;
}

}