#include "errors.hpp"
#include "lua_utils.hpp"

namespace lua {
namespace stack {

/////////////////////////////////////////////////////////////////////////////

class push_t : public boost::static_visitor<void>
{
public :
    typedef push_t self_t;
    push_t(vm_t & vm) : vm_(vm) {};
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
        self_t & self = *this;
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
            self_t & self = *this;
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
            self_t & self = *this;
            lua_pushinteger(vm_.state(), i);
            boost::apply_visitor(self, value[i]);
            lua_settable(vm_.state(), -3);
        }
    }

private :
    vm_t & vm_;
};

/////////////////////////////////////////////////////////////////////////////

void push(vm_t & vm, erlcpp::list_t const& args)
{
    push_t pusher(vm); // (-;
    std::for_each(args.begin(), args.end(), boost::apply_visitor(pusher));
}

/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

erlcpp::term_t peek(vm_t & vm)
{
    switch( lua_type(vm.state(), -1) )
    {
        case LUA_TNIL:
            return erlcpp::atom_t("nil");
        case LUA_TBOOLEAN:
            return erlcpp::atom_t(lua_toboolean(vm.state(), -1) ? "true" : "false");
        case LUA_TNUMBER:
        {
            lua_Number  d = lua_tonumber(vm.state(), -1);
            lua_Integer i = lua_tointeger(vm.state(), -1);
            if (d != i) {
                return erlcpp::num_t(d);
            } else {
                return erlcpp::num_t(i);
            }
        }
        case LUA_TSTRING:
        {
            std::size_t len = 0;
            const char * val = lua_tolstring(vm.state(), -1, &len);
            return erlcpp::binary_t(erlcpp::binary_t::data_t(val, val+len));
        }
        case LUA_TTABLE:
        {
            erlcpp::list_t result;
            lua_pushnil(vm.state());
            while(lua_next(vm.state(), -2))
            {
                erlcpp::term_t val = pop(vm);
                erlcpp::term_t key = peek(vm);
                erlcpp::tuple_t pair(2);
                pair[0] = key;
                pair[1] = val;
                result.push_back(pair);
            }
            return result;
        }
        default :
            throw errors::unsupported_type(lua_typename(vm.state(), lua_type(vm.state(), -1)));
    }
}

erlcpp::term_t pop(vm_t & vm)
{
    erlcpp::term_t result = peek(vm);
    lua_pop(vm.state(), 1);
    return result;
}

erlcpp::term_t pop(vm_t & vm, std::size_t args)
{
    if (args == 1)
    {
        return pop(vm);
    }
    else
    {
        erlcpp::tuple_t result(args);
        while(args)
        {
            result[--args] = pop(vm);
        }
        return result;
    }
}

/////////////////////////////////////////////////////////////////////////////

} // namespace stack
} // namespace lua
