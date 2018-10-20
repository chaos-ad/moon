#include "errors.hpp"
#include "lua_utils.hpp"

namespace lua {
namespace stack {

/////////////////////////////////////////////////////////////////////////////

class push_t : public boost::static_visitor<void>
{
public :
    typedef push_t self_t;
    push_t(lua_State * vm) : vm_(vm) {};
    void operator()(int32_t const& value)
    {
        lua_pushinteger(vm_, value);
    }
    void operator()(int64_t const& value)
    {
        lua_pushnumber(vm_, value);
    }
    void operator()(double const& value)
    {
        lua_pushnumber(vm_, value);
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
        if (value == "true")
        {
            lua_pushboolean(vm_, 1);
        }
        else if (value == "false")
        {
            lua_pushboolean(vm_, 0);
        }
        else if (value == "nil")
        {
            lua_pushnil(vm_);
        }
        else
        {
            lua_pushlstring(vm_, value.c_str(), value.size());
        }
    }

    void operator()(erlcpp::binary_t const& value)
    {
        lua_pushlstring(vm_, value.data(), value.size());
    }

    void operator()(erlcpp::list_t const& value)
    {
        lua_createtable(vm_, value.size(), 0);
        int32_t index = 1;
        erlcpp::list_t::const_iterator i, end = value.end();
        for( i = value.begin(); i != end; ++i )
        {
            try
            {
                erlcpp::tuple_t tuple = boost::get<erlcpp::tuple_t>(*i);
                if (tuple.size() != 2)
                {
                    throw boost::bad_get();
                }
                self_t & self = *this;
                boost::apply_visitor(self, tuple[0]);
                boost::apply_visitor(self, tuple[1]);
                lua_settable(vm_, -3);
            }
            catch(boost::bad_get&)
            {
                self_t & self = *this;
                lua_pushinteger(vm_, index++);
                boost::apply_visitor(self, *i);
                lua_settable(vm_, -3);
            }
        }
    }

    void operator()(erlcpp::tuple_t const& value)
    {
        lua_createtable(vm_, value.size(), 0);
        for( erlcpp::tuple_t::size_type i = 0, end = value.size(); i != end; ++i )
        {
            self_t & self = *this;
            lua_pushinteger(vm_, i+1);
            boost::apply_visitor(self, value[i]);
            lua_settable(vm_, -3);
        }
    }

private :
    lua_State * vm_;
};

/////////////////////////////////////////////////////////////////////////////

void push(lua_State * vm, erlcpp::term_t const& val)
{
    push_t p(vm);
    boost::apply_visitor(p, val);
}

void push_all(lua_State * vm, erlcpp::list_t const& list)
{
    push_t p(vm);
    std::for_each(list.begin(), list.end(), boost::apply_visitor(p));
}

/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

erlcpp::term_t peek(lua_State * vm)
{
    switch( lua_type(vm, -1) )
    {
        case LUA_TNIL:
            return erlcpp::atom_t("nil");
        case LUA_TBOOLEAN:
            return erlcpp::atom_t(lua_toboolean(vm, -1) ? "true" : "false");
        case LUA_TNUMBER:
        {
            lua_Number  d = lua_tonumber(vm, -1);
            lua_Integer i = lua_tointeger(vm, -1);
            if (d != i) {
                return erlcpp::num_t(d);
            } else {
                return erlcpp::num_t(i);
            }
        }
        case LUA_TSTRING:
        {
            std::size_t len = 0;
            const char * val = lua_tolstring(vm, -1, &len);
            return erlcpp::binary_t(erlcpp::binary_t::data_t(val, val+len));
        }
            erlcpp::list_t result;
            lua_pushnil(vm);
            for(int32_t index = 1; lua_next(vm, -2); ++index)
            {
                erlcpp::term_t val = pop(vm);
                erlcpp::term_t key = peek(vm);
                try
                {
                    if (boost::get<LUA_INTEGER>(boost::get<erlcpp::num_t>(key)) == index)
                    {
                        result.push_back(val);
                    }
                    else
                    {
                        erlcpp::tuple_t pair(2);
                        pair[0] = key;
                        pair[1] = val;
                        result.push_back(pair);
                    }
                }
                catch(boost::bad_get&)
                {
                    erlcpp::tuple_t pair(2);
                    pair[0] = key;
                    pair[1] = val;
                    result.push_back(pair);
                }
            }
            return result;
        }
        default :
            throw errors::unsupported_type(lua_typename(vm, lua_type(vm, -1)));
    }
}

erlcpp::term_t pop(lua_State * vm)
{
    erlcpp::term_t result = peek(vm);
    lua_pop(vm, 1);
    return result;
}

erlcpp::term_t pop_all(lua_State * vm)
{
    switch(int N = lua_gettop(vm))
    {
        case 0 : return erlcpp::atom_t("undefined");
        case 1 : return pop(vm);
        default:
        {
            erlcpp::tuple_t result(N);
            while(N)
            {
                result[--N] = pop(vm);
            }
            return result;
        }
    }
}

/////////////////////////////////////////////////////////////////////////////

} // namespace stack
} // namespace lua
