#pragma once

#include <queue>
// #include <boost/thread/mutex.hpp>
// #include <boost/thread/condition_variable.hpp>

#include "erl_nif.h"

// TODO: replace the queue with native ErlNifMutex and ErlNifCond
template<typename data_t>
class queue
{
private:
    std::queue<data_t> queue_;
    // mutable boost::mutex mutex_;
    // boost::condition_variable cond_;
    ErlNifMutex  *lock;
    ErlNifCond  *cond;

public:

    queue()
    {
        char * qlock= "queue_lock";
        char * qcond= "queue_cond";
        lock = enif_mutex_create(&qlock);
        cond = enif_cond_create(&qcond);
    }
    ~queue()
    {
        enif_cond_destroy(cond);
        enif_mutex_destroy(lock);
    }
    void push(data_t const& data)
    {
        // boost::mutex::scoped_lock lock(mutex_);
        enif_mutex_lock(lock);

        queue_.push(data);
        // lock.unlock();
        // cond_.notify_one();
        enif_cond_signal(cond);
        enif_mutex_unlock(lock);
    }

    data_t pop()
    {
        // boost::mutex::scoped_lock lock(mutex_);
        enif_mutex_lock(lock);

        while(queue_.empty())
        {
            enif_cond_wait(cond, lock);
            // cond_.wait(lock);
        }

        data_t result = queue_.front();
        queue_.pop();
        enif_mutex_unlock(lock);
        return result;
    }
};