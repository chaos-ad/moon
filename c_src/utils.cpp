#include "utils.hpp"
#include "errors.hpp"

#include <erl_nif.h>
#include <algorithm>

/////////////////////////////////////////////////////////////////////////////

namespace erlcpp {

/////////////////////////////////////////////////////////////////////////////

template <>
num_t from_erl<num_t>(ErlNifEnv* env, ERL_NIF_TERM term)
{
    int32_t i32;
    if (enif_get_int(env, term, &i32)) {
        return num_t(i32);
    }
    int64_t i64;
    if (enif_get_int64(env, term, &i64)) {
        return num_t(i64);
    }
    double dbl;
    if (enif_get_double(env, term, &dbl)) {
        return num_t(dbl);
    }
    throw errors::invalid_type("invalid_number");
}

template <>
lpid_t from_erl<lpid_t>(ErlNifEnv* env, ERL_NIF_TERM term)
{
    lpid_t result;
    if (!enif_get_local_pid(env, term, result.ptr())) {
        throw errors::invalid_type("invalid_pid");
    }
    return result;
}

template <>
atom_t from_erl<atom_t>(ErlNifEnv* env, ERL_NIF_TERM term)
{
    unsigned int length = 0;
    if (!enif_get_atom_length(env, term, &length, ERL_NIF_LATIN1)) {
        throw errors::invalid_type("invalid_atom");
    }

    std::vector<char> buf(length+1, 0);
    std::size_t sz = enif_get_atom(env, term, buf.data(), buf.size(), ERL_NIF_LATIN1);
    if (sz != buf.size()) {
        throw errors::invalid_type("invalid_atom");
    }

    std::string result(buf.data());
    return atom_t(result);
}

template <>
binary_t from_erl<binary_t>(ErlNifEnv* env, ERL_NIF_TERM term)
{
    ErlNifBinary binary;
    if (!enif_inspect_binary(env, term, &binary)) {
        throw errors::invalid_type("invalid_binary");
    }
    return binary_t(std::vector<char>(binary.data, binary.data + binary.size));
}

template <>
list_t from_erl<list_t>(ErlNifEnv* env, ERL_NIF_TERM term)
{
    list_t result;
    ERL_NIF_TERM head;
    ERL_NIF_TERM tail = term;
    while(!enif_is_empty_list(env, tail))
    {
        if (enif_get_list_cell(env, tail, &head, &tail))
        {
            result.push_back( from_erl<term_t>(env, head) );
        }
        else
        {
            // Handle improper lists:
            result.push_back( from_erl<term_t>(env, tail) );
            break;
        }
    }
    return result;
}

template <>
tuple_t from_erl<tuple_t>(ErlNifEnv* env, ERL_NIF_TERM term)
{
    int arity = 0;
    ERL_NIF_TERM const* tuples;
    if (!enif_get_tuple(env, term, &arity, &tuples)) {
        throw errors::invalid_type("invalid_tuple");
    }

    tuple_t result;
    result.reserve(arity);
    for( int i = 0; i < arity; ++i )
    {
        result.push_back( from_erl<term_t>(env, tuples[i]) );
    }

    return result;
}

template <>
term_t from_erl<term_t>(ErlNifEnv* env, ERL_NIF_TERM term)
{
    if (enif_is_atom(env, term))
    {
        return term_t(from_erl<atom_t>(env, term));
    }
    else if (enif_is_binary(env, term))
    {
        return term_t(from_erl<binary_t>(env, term));
    }
    else if (enif_is_list(env, term))
    {
        return term_t(from_erl<list_t>(env, term));
    }
    else if (enif_is_number(env, term))
    {
        return term_t(from_erl<num_t>(env, term));
    }
    else if (enif_is_pid(env, term))
    {
        return term_t(from_erl<lpid_t>(env, term));
    }
    else if (enif_is_tuple(env, term))
    {
        return term_t(from_erl<tuple_t>(env, term));
    }
    throw errors::invalid_type("unsupported_type");
}

/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

class to_erl_fn : public boost::static_visitor<ERL_NIF_TERM>
{
public :
    to_erl_fn(ErlNifEnv * env) : env(env) {}

    template <class Value>
    ERL_NIF_TERM operator()(Value const& value) const
    {
        return to_erl(env, value);
    }

private :
    ErlNifEnv * env;
};

ERL_NIF_TERM to_erl(ErlNifEnv* env, int32_t value)
{
    return enif_make_int(env, value);
}
ERL_NIF_TERM to_erl(ErlNifEnv* env, int64_t value)
{
    return enif_make_long(env, value);
}
ERL_NIF_TERM to_erl(ErlNifEnv* env, double value)
{
    return enif_make_double(env, value);
}
ERL_NIF_TERM to_erl(ErlNifEnv* env, num_t const& value)
{
    to_erl_fn visitor(env);
    return boost::apply_visitor(visitor, value);
}
ERL_NIF_TERM to_erl(ErlNifEnv* env, lpid_t const& value)
{
    return enif_make_pid(env, value.ptr());
}
ERL_NIF_TERM to_erl(ErlNifEnv* env, atom_t const& value)
{
    return enif_make_atom(env, value.c_str());
}
ERL_NIF_TERM to_erl(ErlNifEnv* env, binary_t const& value)
{
    ErlNifBinary binary;
    if (!enif_alloc_binary(value.size(), &binary)) {
        throw errors::enomem();
    }
    std::copy(value.begin(), value.end(), binary.data);
    return enif_make_binary(env, &binary);
}
ERL_NIF_TERM to_erl(ErlNifEnv* env, list_t const& value)
{
    std::vector<ERL_NIF_TERM> terms;
    terms.reserve(value.size());
    to_erl_fn visitor(env);
    std::transform
    (
        value.begin(), value.end(),
        std::back_inserter(terms),
        boost::apply_visitor(visitor)
    );
    return enif_make_list_from_array(env, terms.data(), terms.size());
}
ERL_NIF_TERM to_erl(ErlNifEnv* env, tuple_t const& value)
{
    std::vector<ERL_NIF_TERM> terms;
    terms.reserve(value.size());
    to_erl_fn visitor(env);
    std::transform
    (
        value.begin(), value.end(),
        std::back_inserter(terms),
        boost::apply_visitor(visitor)
    );
    return enif_make_tuple_from_array(env, terms.data(), terms.size());
}
ERL_NIF_TERM to_erl(ErlNifEnv* env, term_t const& value)
{
    to_erl_fn visitor(env);
    return boost::apply_visitor(visitor, value);
}

}
