#include "lua.hpp"
#include "utils.hpp"
#include "errors.hpp"
#include "lua_utils.hpp"

namespace lua {

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
    void operator()(vm_t::tasks::load_t const& load)
    {
        stack_guard_t guard(vm());
        try
        {
            std::string file(load.file.data(), load.file.data() + load.file.size());
            if (luaL_dofile(vm().state(), file.c_str()))
            {
                erlcpp::tuple_t result(2);
                result[0] = erlcpp::atom_t("error");
                result[1] = lua::stack::pop(vm().state());
                send_result(vm(), "moon_response", result);
            }
            else
            {
                erlcpp::atom_t result("ok");
                send_result(vm(), "moon_response", result);
            }
        }
        catch( std::exception & ex )
        {
            erlcpp::tuple_t result(2);
            result[0] = erlcpp::atom_t("error");
            result[1] = erlcpp::atom_t(ex.what());
            send_result(vm(), "moon_response", result);
        }
    }

    // Evaluating arbitrary code:
    void operator()(vm_t::tasks::eval_t const& eval)
    {
        stack_guard_t guard(vm());
        try
        {
            if ( luaL_loadbuffer(vm().state(), eval.code.data(), eval.code.size(), "line") ||
                    lua_pcall(vm().state(), 0, LUA_MULTRET, 0) )
            {
                erlcpp::tuple_t result(2);
                result[0] = erlcpp::atom_t("error");
                result[1] = lua::stack::pop(vm().state());
                send_result(vm(), "moon_response", result);
            }
            else
            {
                erlcpp::tuple_t result(2);
                result[0] = erlcpp::atom_t("ok");
                result[1] = lua::stack::pop_all(vm().state());
                send_result(vm(), "moon_response", result);
            }
        }
        catch( std::exception & ex )
        {
            erlcpp::tuple_t result(2);
            result[0] = erlcpp::atom_t("error");
            result[1] = erlcpp::atom_t(ex.what());
            send_result(vm(), "moon_response", result);
        }
    }

    // Calling arbitrary function:
    void operator()(vm_t::tasks::call_t const& call)
    {
        stack_guard_t guard(vm());
        try
        {
            lua_getglobal(vm().state(), call.fun.c_str());

            lua::stack::push_all(vm().state(), call.args);

            if (lua_pcall(vm().state(), call.args.size(), LUA_MULTRET, 0))
            {
                erlcpp::tuple_t result(2);
                result[0] = erlcpp::atom_t("error");
                result[1] = lua::stack::pop(vm().state());
                send_result(vm(), "moon_response", result);
            }
            else
            {
                erlcpp::tuple_t result(2);
                result[0] = erlcpp::atom_t("ok");
                result[1] = lua::stack::pop_all(vm().state());
                send_result(vm(), "moon_response", result);
            }
        }
        catch( std::exception & ex )
        {
            erlcpp::tuple_t result(2);
            result[0] = erlcpp::atom_t("error");
            result[1] = erlcpp::atom_t(ex.what());
            send_result(vm(), "moon_response", result);
        }
    }
};

/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

int erlang_call(vm_t & vm)
{
    bool exception_caught = false; // because lua_error makes longjump
    try
    {
        stack_guard_t guard(vm);

        erlcpp::term_t args = lua::stack::pop_all(vm.state());

        send_result(vm, "moon_callback", args);
        erlcpp::term_t result = perform_task<result_handler>(vm);

        lua::stack::push(vm.state(), result);

        guard.dismiss();
        return 1;
    }
    catch(std::exception & ex)
    {
        lua::stack::push(vm.state(), erlcpp::atom_t(ex.what()));
        exception_caught = true;
    }

    if (exception_caught) {
        lua_error(vm.state());
    }

    return 0;
}

extern "C"
{
    static int erlang_call(lua_State * vm)
    {
        int index = lua_upvalueindex(1);
        assert(lua_islightuserdata(vm, index));
        void * data = lua_touserdata(vm, index);
        assert(data);
        return erlang_call(*static_cast<vm_t*>(data));
    }
    
    static const struct luaL_Reg erlang_lib[] =
    {
        {"call", erlang_call},
        {NULL, NULL}
    };
}

vm_t::vm_t(erlcpp::lpid_t const& pid)
    : pid_(pid)
    , luastate_(luaL_newstate(), lua_close)
{
    luaL_openlibs(luastate_.get());
    lua_newtable(luastate_.get());
    lua_pushlightuserdata(luastate_.get(), this);
    luaL_setfuncs(luastate_.get(), erlang_lib, 1);
    lua_setglobal(luastate_.get(), "erlang");
}

vm_t::~vm_t()
{
//     enif_fprintf(stderr, "*** destruct the vm\n");
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

/////////////////////////////////////////////////////////////////////////////

}
