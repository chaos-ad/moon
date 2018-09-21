#pragma once

#include <stdexcept>

/////////////////////////////////////////////////////////////////////////////

namespace errors {

struct badarg : std::exception {};
struct enomem : public std::runtime_error
{
    enomem() : std::runtime_error("enomem") {}
};

struct unsupported_type : public std::runtime_error
{
    unsupported_type() : std::runtime_error("unsupported_type") {}
    unsupported_type(std::string const& msg) : std::runtime_error(msg) {}
};

struct invalid_type : public std::runtime_error
{
    invalid_type() : std::runtime_error("invalid_type") {}
    invalid_type(std::string const& msg) : std::runtime_error(msg) {}
};

struct unexpected_msg : public std::runtime_error
{
    unexpected_msg() : std::runtime_error("unexpected_msg") {};
};

}

/////////////////////////////////////////////////////////////////////////////
