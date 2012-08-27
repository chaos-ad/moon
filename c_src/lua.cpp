#include "lua.hpp"
#include "utils.hpp"
#include "errors.hpp"

namespace lua {

/////////////////////////////////////////////////////////////////////////////

class quit_tag {};

/////////////////////////////////////////////////////////////////////////////

class stack_guard_t
{
public :
    stack_guard_t(vm_t & vm)
        : vm_(vm)
        , top_(lua_gettop(vm_.state()))
    {};
    ~stack_guard_t()
    {
        lua_settop(vm_.state(), top_);
    }
private :
    vm_t & vm_;
    int top_;
};

/////////////////////////////////////////////////////////////////////////////

template <class worker_t>
typename worker_t::result_type
perform_task(vm_t & vm)
{
    vm_t::task_t task = vm.get_task();
    worker_t worker(vm);
    return boost::apply_visitor(worker, task);
}

template <class result_t>
void send_result(vm_t & vm, std::string const& type, result_t const& result)
{
    boost::shared_ptr<ErlNifEnv> env(enif_alloc_env(), enif_free_env);
    erlcpp::tuple_t packet(2);
    packet[0] = erlcpp::atom_t(type);
    packet[1] = result;
    enif_send(NULL, vm.erl_pid().ptr(), env.get(), erlcpp::to_erl(env.get(), packet));
}

/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
// task handlers:
template <class return_type>
class base_handler : public boost::static_visitor<return_type>
{
public :
    typedef typename boost::static_visitor<return_type>::result_type result_type;

    base_handler(vm_t & vm) : vm_(vm) {};
    virtual return_type operator()(vm_t::tasks::load_t const&) { throw errors::unexpected_msg(); }
    virtual return_type operator()(vm_t::tasks::eval_t const&) { throw errors::unexpected_msg(); }
    virtual return_type operator()(vm_t::tasks::call_t const&) { throw errors::unexpected_msg(); }
    virtual return_type operator()(vm_t::tasks::resp_t const&) { throw errors::unexpected_msg(); }
    virtual return_type operator()(vm_t::tasks::quit_t const&) { throw quit_tag(); }

    vm_t & vm() { return vm_; };
    vm_t const& vm() const { return vm_; }

protected :
    ~base_handler() {}

private :
    vm_t & vm_;
};

/////////////////////////////////////////////////////////////////////////////

class result_handler : public base_handler<erlcpp::term_t>
{
public :
    using base_handler::operator();
    result_handler(vm_t & vm) : base_handler(vm) {};
    erlcpp::term_t operator()(vm_t::tasks::resp_t const& resp) { return resp.term; }
};

struct call_handler : public base_handler<void>
{
    using base_handler::operator();
    call_handler(vm_t & vm) : base_handler(vm) {};

    // Loading file:
    virtual void operator()(vm_t::tasks::load_t const& load)
    {
        stack_guard_t guard(vm());
        std::string file(load.file.data(), load.file.data() + load.file.size());
        if (luaL_dofile(vm().state(), file.c_str()))
        {
            erlcpp::tuple_t result(2);
            result[0] = erlcpp::atom_t("error");
            result[1] = erlcpp::binary_t(lua_tostring(vm().state(), -1));
            send_result(vm(), "moon_response", result);
        }
        else
        {
            erlcpp::atom_t result("ok");
            send_result(vm(), "moon_response", result);
        }
    }

    // Evaluating arbitrary code:
    void operator()(vm_t::tasks::eval_t const& eval)
    {
        stack_guard_t guard(vm());
        std::string code(eval.code.data(), eval.code.data() + eval.code.size());
        enif_fprintf(stderr, "*** eval task: %s\n", code.c_str());

        if ( luaL_loadbuffer(vm().state(), eval.code.data(), eval.code.size(), "line") ||
                   lua_pcall(vm().state(), 0, LUA_MULTRET, 0) )
        {
            enif_fprintf(stderr, "*** evaluation failed: %s\n", lua_tostring(vm().state(), -1));
            erlcpp::tuple_t result(2);
            result[0] = erlcpp::atom_t("error");
            result[1] = erlcpp::binary_t(lua_tostring(vm().state(), -1));
            send_result(vm(), "moon_response", result);
        }
        else
        {
            int top = lua_gettop(vm().state());
            enif_fprintf(stderr, "*** evaluating %s: new stack top: %d\n", code.c_str(), top);
            erlcpp::atom_t result("ok");
            send_result(vm(), "moon_response", result);
        }
    }

    // Calling arbitrary function:
    void operator()(vm_t::tasks::call_t const& call)
    {
        stack_guard_t guard(vm());
        enif_fprintf(stderr, "*** call task: %s\n", call.fun.c_str());

        lua_getglobal(vm().state(), call.fun.c_str());
        if (lua_pcall(vm().state(), 0, LUA_MULTRET, 0))
        {
            enif_fprintf(stderr, "*** call failed: %s\n", lua_tostring(vm().state(), -1));
            erlcpp::tuple_t result(2);
            result[0] = erlcpp::atom_t("error");
            result[1] = erlcpp::binary_t(lua_tostring(vm().state(), -1));
            send_result(vm(), "moon_response", result);
        }
        else
        {
            int top = lua_gettop(vm().state());
            enif_fprintf(stderr, "*** calling %s: new stack top: %d\n", call.fun.c_str(), top);
            erlcpp::atom_t result("ok");
            send_result(vm(), "moon_response", result);
        }
    }
};

/////////////////////////////////////////////////////////////////////////////

vm_t::vm_t(erlcpp::lpid_t const& pid)
    : pid_(pid)
    , luastate_(::luaL_newstate(), ::lua_close)
{
    ::luaL_openlibs(luastate_.get()),
    enif_fprintf(stderr, "*** construct the vm\n");
}

vm_t::~vm_t()
{
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

lua_State* vm_t::state()
{
    return luastate_.get();
}

lua_State const* vm_t::state() const
{
    return luastate_.get();
}

void* vm_t::thread_run(void * vm)
{
    static_cast<vm_t*>(vm)->run();
    return 0;
}

}