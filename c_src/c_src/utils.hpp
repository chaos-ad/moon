#pragma once

#include "types.hpp"

/////////////////////////////////////////////////////////////////////////////

namespace erlcpp {

/////////////////////////////////////////////////////////////////////////////

template <class T>
T from_erl(ErlNifEnv* env, ERL_NIF_TERM term);

template <> num_t    from_erl<num_t>(ErlNifEnv* env, ERL_NIF_TERM term);
template <> lpid_t   from_erl<lpid_t>(ErlNifEnv* env, ERL_NIF_TERM term);
template <> atom_t   from_erl<atom_t>(ErlNifEnv* env, ERL_NIF_TERM term);
template <> binary_t from_erl<binary_t>(ErlNifEnv* env, ERL_NIF_TERM term);
template <> list_t   from_erl<list_t>(ErlNifEnv* env, ERL_NIF_TERM term);
template <> tuple_t  from_erl<tuple_t>(ErlNifEnv* env, ERL_NIF_TERM term);
template <> term_t   from_erl<term_t>(ErlNifEnv* env, ERL_NIF_TERM term);

/////////////////////////////////////////////////////////////////////////////

ERL_NIF_TERM to_erl(ErlNifEnv* env, int32_t value);
ERL_NIF_TERM to_erl(ErlNifEnv* env, int64_t value);
ERL_NIF_TERM to_erl(ErlNifEnv* env, double value);
ERL_NIF_TERM to_erl(ErlNifEnv* env, num_t const& value);
ERL_NIF_TERM to_erl(ErlNifEnv* env, lpid_t const& value);
ERL_NIF_TERM to_erl(ErlNifEnv* env, atom_t const& value);
ERL_NIF_TERM to_erl(ErlNifEnv* env, binary_t const& value);
ERL_NIF_TERM to_erl(ErlNifEnv* env, list_t const& value);
ERL_NIF_TERM to_erl(ErlNifEnv* env, tuple_t const& value);
ERL_NIF_TERM to_erl(ErlNifEnv* env, term_t const& value);

/////////////////////////////////////////////////////////////////////////////

}
