// #pragma once
//
// #include <set>
// #include <map>
// #include <list>
// #include <vector>
// #include <limits>
// #include <sstream>
// #include <iostream>
// #include <stdexcept>
// #include <exception>
//
// #include <boost/variant.hpp>
// #include <boost/optional.hpp>
// #include <boost/mpl/identity.hpp>
//
// #include "erl_nif.h"
//
// /////////////////////////////////////////////////////////////////////////////
//
// namespace erlang {
//
// struct list_t : public std::vector<ERL_NIF_TERM>
// {
//     typedef std::vector<ERL_NIF_TERM> base_t;
//
//     template <class InputIterator>
//     tuple_t(InputIterator const& begin, InputIterator const& end) : base_t(begin, end) {};
// };
//
// class tuple_t : public std::vector<ERL_NIF_TERM>
// {
// public :
//     typedef std::vector<ERL_NIF_TERM> base_t;
//
//     template <class InputIterator>
//     tuple_t(InputIterator const& begin, InputIterator const& end) : base_t(begin, end) {};
// };
//
// struct atom_t : std::string
// {
//     atom_t(std::string const& str) : std::string(str) {}
// };
//
// class local_pid_t
// {
// public :
//     local_pid_t() {};
//     ErlNifPid* ptr() { return &pid; }
// private:
//     ErlNifPid pid;
// };
//
// struct enomem : public std::runtime_error
// {
//     enomem() : runtime_error("enomem") {}
// };
//
// struct invalid_type : public std::runtime_error
// {
//     invalid_type() : runtime_error("invalid_type") {}
//     invalid_type(std::string const& msg) : runtime_error(msg) {}
// };
//
// /////////////////////////////////////////////////////////////////////////////
//
// template <typename T>
// T erl2cpp(ErlNifEnv* env, ERL_NIF_TERM term);
//
// /////////////////////////////////////////////////////////////////////////////
//
// inline bool erl2cpp(ErlNifEnv* env, ERL_NIF_TERM term, boost::mpl::identity<bool>)
// {
//     atom_t atom = erl2cpp<atom_t>(env, term);
//     if (atom ==  "true") return true;
//     if (atom == "false") return false;
//     throw invalid_type();
// };
//
// inline int16_t erl2cpp(ErlNifEnv* env, ERL_NIF_TERM term, boost::mpl::identity<int16_t>)
// {
//     int32_t result;
//     if (!enif_get_int(env, term, &result)) {
//         throw invalid_type();
//     }
//     if (result > std::numeric_limits<int16_t>::max()) {
//         throw invalid_type();
//     }
//     return result;
// };
//
// inline int32_t erl2cpp(ErlNifEnv* env, ERL_NIF_TERM term, boost::mpl::identity<int32_t>)
// {
//     int32_t result;
//     if (!enif_get_int(env, term, &result)) {
//         throw invalid_type();
//     }
//     return result;
// }
//
// inline int64_t erl2cpp(ErlNifEnv* env, ERL_NIF_TERM term, boost::mpl::identity<int64_t>)
// {
//     int64_t result;
//     if (!enif_get_int64(env, term, &result)) {
//         throw invalid_type();
//     }
//     return result;
// }
//
// inline double erl2cpp(ErlNifEnv* env, ERL_NIF_TERM term, boost::mpl::identity<double>)
// {
//     double result;
//     if (!enif_get_double(env, term, &result)) {
//         return erl2cpp<int64_t>(env, term);
//     }
//     return result;
// }
//
// inline std::string erl2cpp(ErlNifEnv* env, ERL_NIF_TERM term, boost::mpl::identity<std::string>)
// {
//     // Try binary first
//     ErlNifBinary binary;
//     if (enif_inspect_binary(env, term, &binary)) {
//         return std::string(reinterpret_cast<char*>(binary.data), binary.size);
//     }
//
//     // Then try a list
//     unsigned int length = 0;
//     if (enif_get_list_length(env, term, &length)) {
//         std::vector<char> buf(length+1, 0);
//         std::size_t sz = enif_get_string(env, term, buf.data(), buf.size(), ERL_NIF_LATIN1);
//         if (sz != buf.size()) {
//             throw invalid_type();
//         }
//
//         return std::string(buf.data());
//     }
//
//     throw invalid_type();
// }
//
// inline atom_t erl2cpp(ErlNifEnv* env, ERL_NIF_TERM term, boost::mpl::identity<atom_t>)
// {
//     unsigned int length = 0;
//     if (!enif_get_atom_length(env, term, &length, ERL_NIF_LATIN1)) {
//         throw invalid_type();
//     }
//
//     std::vector<char> buf(length+1, 0);
//     std::size_t sz = enif_get_atom(env, term, buf.data(), buf.size(), ERL_NIF_LATIN1);
//     if (sz != buf.size()) {
//         throw invalid_type();
//     }
//
//     std::string result(buf.data());
//     return atom_t(result);
// }
//
// inline local_pid_t erl2cpp(ErlNifEnv* env, ERL_NIF_TERM term, boost::mpl::identity<local_pid_t>)
// {
//     local_pid_t pid;
//     if (!enif_get_local_pid(env, term, pid.ptr())) {
//         throw invalid_type("invalid_pid");
//     }
//     return pid;
// }
//
// inline tuple_t erl2cpp(ErlNifEnv* env, ERL_NIF_TERM term, boost::mpl::identity<tuple_t>)
// {
//     int arity = 0;
//     ERL_NIF_TERM const* result;
//     if (!enif_get_tuple(env, term, &arity, &result)) {
//         throw invalid_type();
//     }
//
//     return tuple_t(result, result+arity);
// }
//
// template <typename T>
// inline std::set<T> erl2cpp(ErlNifEnv* env, ERL_NIF_TERM term, boost::mpl::identity< std::set<T> >)
// {
//     std::set<T> result;
//     ERL_NIF_TERM head;
//     ERL_NIF_TERM tail = term;
//     while(!enif_is_empty_list(env, tail))
//     {
//         if (!enif_get_list_cell(env, tail, &head, &tail))
//         {
//             throw invalid_type();
//         }
//         result.insert( erl2cpp<T>(env, head) );
//     }
//     return result;
// }
//
// template <typename T>
// inline std::list<T> erl2cpp(ErlNifEnv* env, ERL_NIF_TERM term, boost::mpl::identity< std::list<T> >)
// {
//     std::list<T> result;
//     ERL_NIF_TERM head;
//     ERL_NIF_TERM tail = term;
//     while(!enif_is_empty_list(env, tail))
//     {
//         if (!enif_get_list_cell(env, tail, &head, &tail))
//         {
//             throw invalid_type();
//         }
//         result.push_back( erl2cpp<T>(env, head) );
//     }
//     return result;
// }
//
// template <typename T>
// inline std::vector<T> erl2cpp(ErlNifEnv* env, ERL_NIF_TERM term, boost::mpl::identity< std::vector<T> >)
// {
//     std::vector<T> result;
//     ERL_NIF_TERM head;
//     ERL_NIF_TERM tail = term;
//     while(!enif_is_empty_list(env, tail))
//     {
//         if (!enif_get_list_cell(env, tail, &head, &tail))
//         {
//             throw invalid_type();
//         }
//         result.push_back( erl2cpp<T>(env, head) );
//     }
//     return result;
// }
//
// template <typename K, typename V>
// inline std::map<K, V> erl2cpp(ErlNifEnv* env, ERL_NIF_TERM term, boost::mpl::identity< std::map<K, V> >)
// {
//     std::map<K, V> result;
//     std::list<tuple_t> list = erl2cpp< std::list<tuple_t> >(env, term);
//     std::list<tuple_t>::const_iterator i, end = list.end();
//     for( i = list.begin(); i != end; ++i ) {
//         result.insert( std::make_pair(
//             erl2cpp<K>(env, i->at(0)),
//             erl2cpp<V>(env, i->at(1)) )
//         );
//     }
//     return result;
// }
//
// template <typename T>
// inline T erl2cpp(ErlNifEnv* env, ERL_NIF_TERM term)
// {
//     return erl2cpp(env, term, boost::mpl::identity<T>());
// }
//
// /////////////////////////////////////////////////////////////////////////////
// /////////////////////////////////////////////////////////////////////////////
//
// inline ERL_NIF_TERM cpp2erl(ErlNifEnv* env, bool value)
// {
//     if (value) {
//         return enif_make_atom(env, "true");
//     } else {
//         return enif_make_atom(env, "false");
//     }
// }
//
// inline ERL_NIF_TERM cpp2erl(ErlNifEnv* env, int16_t value)
// {
//     return enif_make_int(env, value);
// }
//
// inline ERL_NIF_TERM cpp2erl(ErlNifEnv* env, int32_t value)
// {
//     return enif_make_int(env, value);
// }
//
// inline ERL_NIF_TERM cpp2erl(ErlNifEnv* env, int64_t value)
// {
//     return enif_make_long(env, value);
// }
//
// inline ERL_NIF_TERM cpp2erl(ErlNifEnv* env, double value)
// {
//     return enif_make_double(env, value);
// }
//
// inline ERL_NIF_TERM cpp2erl(ErlNifEnv* env, std::string const& value)
// {
//     ErlNifBinary binary;
//     if (!enif_alloc_binary(value.size(), &binary)) {
//         throw enomem();
//     }
//     std::copy(value.begin(), value.end(), binary.data);
//     return enif_make_binary(env, &binary);
// }
//
// inline ERL_NIF_TERM cpp2erl(ErlNifEnv* env, atom_t const& value)
// {
//     return enif_make_atom(env, value.c_str());
// }
//
// inline ERL_NIF_TERM cpp2erl(ErlNifEnv* env, tuple_t const& value)
// {
//     return enif_make_tuple_from_array(env, value.data(), value.size());
// }
//
//
// template <class T>
// inline ERL_NIF_TERM cpp2erl(ErlNifEnv* env, std::set<T> const& value)
// {
//     std::vector<ERL_NIF_TERM> terms;
//     typename std::set<T>::const_iterator i, end = value.end();
//     for( i = value.begin(); i != end; ++i )
//     {
//         terms.push_back( cpp2erl(env, *i) );
//     }
//     return enif_make_list_from_array(env, terms.data(), terms.size());
// }
//
// template <class T>
// inline ERL_NIF_TERM cpp2erl(ErlNifEnv* env, std::list<T> const& value)
// {
//     std::vector<ERL_NIF_TERM> terms;
//     typename std::list<T>::const_iterator i, end = value.end();
//     for( i = value.begin(); i != end; ++i )
//     {
//         terms.push_back( cpp2erl(env, *i) );
//     }
//     return enif_make_list_from_array(env, terms.data(), terms.size());
// }
//
// template <class T>
// inline ERL_NIF_TERM cpp2erl(ErlNifEnv* env, std::vector<T> const& value)
// {
//     std::vector<ERL_NIF_TERM> terms;
//     typename std::vector<T>::const_iterator i, end = value.end();
//     for( i = value.begin(); i != end; ++i )
//     {
//         terms.push_back( cpp2erl(env, *i) );
//     }
//     return enif_make_list_from_array(env, terms.data(), terms.size());
// }
//
// template <class K, class V>
// inline ERL_NIF_TERM cpp2erl(ErlNifEnv* env, std::map<K, V> const& value)
// {
//     std::vector<ERL_NIF_TERM> terms;
//     typename std::map<K, V>::const_iterator i, end = value.end();
//     for( i = value.begin(); i != end; ++i )
//     {
//         std::vector<ERL_NIF_TERM> tuple(2);
//         tuple[0] = cpp2erl(env, i->first);
//         tuple[1] = cpp2erl(env, i->second);
//         terms.push_back( cpp2erl(env, tuple) );
//     }
//     return enif_make_list_from_array(env, terms.data(), terms.size());
// }
//
// /////////////////////////////////////////////////////////////////////////////
//
// inline void check_record(ErlNifEnv * env, ERL_NIF_TERM term, std::string atom)
// {
//     atom_t recordname = erl2cpp<atom_t>(env, term);
//     if ( recordname != atom )
//     {
//         std::stringstream out;
//         out << "Invalid record: " << recordname << "; expected: " << atom << std::endl;
//         throw std::runtime_error(out.str());
//     }
// }
//
// /////////////////////////////////////////////////////////////////////////////
//
// }
