#include "types.hpp"
#include "errors.hpp"

/////////////////////////////////////////////////////////////////////////////

namespace erlcpp {

/////////////////////////////////////////////////////////////////////////////

num_t    get_num(ErlNifEnv* env, ERL_NIF_TERM term);
lpid_t   get_pid(ErlNifEnv* env, ERL_NIF_TERM term);
atom_t   get_atom(ErlNifEnv* env, ERL_NIF_TERM term);
binary_t get_binary(ErlNifEnv* env, ERL_NIF_TERM term);
list_t   get_list(ErlNifEnv* env, ERL_NIF_TERM term);
tuple_t  get_tuple(ErlNifEnv* env, ERL_NIF_TERM term);
term_t   get_term(ErlNifEnv* env, ERL_NIF_TERM term);

/////////////////////////////////////////////////////////////////////////////

void validate_args(int argc, int neeeded);

}
