#include "types.hpp"
#include "queue.hpp"

#include <lua.hpp>
#include <boost/shared_ptr.hpp>

namespace lua {

class vm_t
{
private:
    vm_t(erlcpp::lpid_t const& pid);
    ~vm_t();

    void run();
    void stop();

    static void* thread_run(void * vm);

public :
    struct tasks
    {
        struct call_t
        {
            call_t(erlcpp::atom_t const& fun, erlcpp::list_t const& args)
                : fun(fun), args(args)
            {};
            erlcpp::atom_t fun;
            erlcpp::list_t args;
        };
        struct resp_t
        {
            resp_t(erlcpp::term_t const& term) : term(term) {}
            erlcpp::term_t term;
        };
        struct quit_t {};
    };
    typedef boost::variant<tasks::call_t, tasks::resp_t, tasks::quit_t> task_t;

public :

    erlcpp::lpid_t erl_pid() const { return pid_; }

    void add_task(task_t const& task);

    static void destroy(ErlNifEnv* env, void* obj);
    static boost::shared_ptr<vm_t> create(ErlNifResourceType* res_type, erlcpp::lpid_t const& pid);

private :
    erlcpp::lpid_t      pid_;
    ErlNifTid           tid_;
    lua_State       *   luastate_;
    queue<task_t>       queue_;
};

}
