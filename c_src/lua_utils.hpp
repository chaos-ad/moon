#pragma once

#include "lua.hpp"
#include "utils.hpp"

namespace lua {

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

class quit_tag {};

class stack_guard_t
{
public :
    stack_guard_t(vm_t & vm) : vm_(vm), top_(lua_gettop(vm_.state())) {};
    ~stack_guard_t() { lua_settop(vm_.state(), top_); }
private :
    vm_t & vm_;
    int top_;
};

/////////////////////////////////////////////////////////////////////////////

namespace stack
{
    erlcpp::term_t pop(lua_State * vm);
    erlcpp::term_t pop(lua_State * vm, std::size_t num);
    void push(lua_State * vm, erlcpp::list_t const& args);

    inline erlcpp::term_t pop(vm_t & vm) { return pop(vm.state()); }
    inline erlcpp::term_t pop(vm_t & vm, std::size_t num) { return pop(vm.state(), num); }
    inline void push(vm_t & vm, erlcpp::list_t const& args) { return push(vm.state(), args); }
}

/////////////////////////////////////////////////////////////////////////////

}