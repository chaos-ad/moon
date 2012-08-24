#include "utils.hpp"

#include <erl_nif.h>

/////////////////////////////////////////////////////////////////////////////

namespace erlcpp {

/////////////////////////////////////////////////////////////////////////////

num_t get_num(ErlNifEnv* env, ERL_NIF_TERM term)
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

lpid_t get_pid(ErlNifEnv* env, ERL_NIF_TERM term)
{
    lpid_t result;
    if (!enif_get_local_pid(env, term, result.ptr())) {
        throw errors::invalid_type("invalid_pid");
    }
    return result;
}

atom_t get_atom(ErlNifEnv* env, ERL_NIF_TERM term)
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

binary_t get_binary(ErlNifEnv* env, ERL_NIF_TERM term)
{
    ErlNifBinary binary;
    if (!enif_inspect_binary(env, term, &binary)) {
        throw errors::invalid_type("invalid_binary");
    }
    return binary_t(std::vector<char>(binary.data, binary.data + binary.size));
}

list_t get_list(ErlNifEnv* env, ERL_NIF_TERM term)
{
    list_t result;
    ERL_NIF_TERM head;
    ERL_NIF_TERM tail = term;
    while(!enif_is_empty_list(env, tail))
    {
        if (!enif_get_list_cell(env, tail, &head, &tail))
        {
            throw errors::invalid_type("invalid_list");
        }
        result.push_back( get_term(env, head) );
    }
    return result;
}

tuple_t get_tuple(ErlNifEnv* env, ERL_NIF_TERM term)
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
        result.push_back( get_term(env, tuples[i]) );
    }

    return result;
}

term_t get_term(ErlNifEnv* env, ERL_NIF_TERM term)
{
    if (enif_is_atom(env, term))
    {
        return term_t(get_atom(env, term));
    }
    else if (enif_is_binary(env, term))
    {
        return term_t(get_binary(env, term));
    }
    else if (enif_is_list(env, term))
    {
        return term_t(get_list(env, term));
    }
    else if (enif_is_number(env, term))
    {
        return term_t(get_num(env, term));
    }
    else if (enif_is_pid(env, term))
    {
        return term_t(get_pid(env, term));
    }
    else if (enif_is_tuple(env, term))
    {
        return term_t(get_tuple(env, term));
    }
    throw errors::invalid_type("unsupported_type");
}

/////////////////////////////////////////////////////////////////////////////

void validate_args(int argc, int needed)
{
    if (argc != needed)
    {
        throw errors::badarg();
    }
}

}
