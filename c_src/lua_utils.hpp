#pragma once

#include "lua.hpp"

namespace lua {

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

class push_args_t : public boost::static_visitor<void>
{
public :
    push_args_t(vm_t & vm) : vm_(vm) {};
    void operator()(int32_t const& value)
    {
        lua_pushinteger(vm_.state(), value);
    }
    void operator()(int64_t const& value)
    {
        lua_pushnumber(vm_.state(), value);
    }
    void operator()(double const& value)
    {
        lua_pushnumber(vm_.state(), value);
    }
    void operator()(erlcpp::num_t const& value)
    {
        push_args_t & self = *this;
        boost::apply_visitor(self, value);
    }
    void operator()(erlcpp::lpid_t const& value)
    {
        throw errors::unsupported_type();
    }
    void operator()(erlcpp::atom_t const& value)
    {
        lua_pushlstring(vm_.state(), value.c_str(), value.size());
    }

    void operator()(erlcpp::binary_t const& value)
    {
        lua_pushlstring(vm_.state(), value.data(), value.size());
    }

    void operator()(erlcpp::list_t const& value)
    {
        lua_createtable(vm_.state(), value.size(), 0);
        erlcpp::list_t::const_iterator i, end = value.end();
        for( i = value.begin(); i != end; ++i )
        {
            erlcpp::tuple_t tuple = boost::get<erlcpp::tuple_t>(*i);
            if (tuple.size() != 2)
            {
                throw errors::unsupported_type();
            }
            push_args_t & self = *this;
            boost::apply_visitor(self, tuple[0]);
            boost::apply_visitor(self, tuple[1]);
            lua_settable(vm_.state(), -3);
        }
    }

    void operator()(erlcpp::tuple_t const& value)
    {
        lua_createtable(vm_.state(), value.size(), 0);
        for( erlcpp::tuple_t::size_type i = 0, end = value.size(); i != end; ++i )
        {
            push_args_t & self = *this;
            lua_pushinteger(vm_.state(), i);
            boost::apply_visitor(self, value[i]);
            lua_settable(vm_.state(), -3);
        }
    }

private :
    vm_t & vm_;
};

/////////////////////////////////////////////////////////////////////////////

}