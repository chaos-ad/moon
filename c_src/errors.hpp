#pragma once

#include <stdexcept>

/////////////////////////////////////////////////////////////////////////////

namespace erlcpp { namespace errors {

struct enomem : public std::runtime_error
{
    enomem() : std::runtime_error("enomem") {}
};

struct invalid_type : public std::runtime_error
{
    invalid_type() : std::runtime_error("invalid_type") {}
    invalid_type(std::string const& msg) : std::runtime_error(msg) {}
};

}}

/////////////////////////////////////////////////////////////////////////////
