#pragma once

#include <queue>
#include <boost/thread/mutex.hpp>
#include <boost/thread/condition_variable.hpp>

// TODO: replace the queue with native ErlNifMutex and ErlNifCond
template<typename data_t>
class queue
{
private:
    std::queue<data_t> queue_;
    mutable boost::mutex mutex_;
    boost::condition_variable cond_;
public:
    void push(data_t const& data)
    {
        boost::mutex::scoped_lock lock(mutex_);
        queue_.push(data);
        lock.unlock();
        cond_.notify_one();
    }

    data_t pop()
    {
        boost::mutex::scoped_lock lock(mutex_);
        while(queue_.empty())
        {
            cond_.wait(lock);
        }

        data_t result = queue_.front();
        queue_.pop();
        return result;
    }
};