#include "types.hpp"
#include "queue.hpp"
#include <boost/shared_ptr.hpp>

#include <lua.h>

namespace lua {

class vm_t
{
public:
    vm_t();
    ~vm_t();

    void run();
    static void* run_vm(void * vm);

public :
    struct tasks
    {
        struct call_t
        {
            call_t
            (
                erlcpp::lpid_t const& pid,
                erlcpp::atom_t const& fun,
                erlcpp::list_t const& args
            )
                : pid(pid), fun(fun), args(args)
            {};

            erlcpp::lpid_t pid;
            erlcpp::atom_t fun;
            erlcpp::list_t args;
        };
        struct quit_t {};
    };
    typedef boost::variant<tasks::call_t, tasks::quit_t> task_t;

public :
    void stop();
    void add_task(task_t const& task);

    static void destroy(ErlNifEnv* env, void* obj);
    static boost::shared_ptr<vm_t> create(ErlNifResourceType* res_type);

private :
    ErlNifTid           tid_;
    lua_State       *   luastate_;
    queue<task_t>       queue_;
};

}
