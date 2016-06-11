/**
 * @file utils.hpp
 * @author Konrad Zemek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.md'
 */

#ifndef ONE_ETLS_UTILS_HPP
#define ONE_ETLS_UTILS_HPP

#ifdef _GNU_SOURCE
#include <pthread.h>
#endif

#include <string>
#include <thread>

namespace one {
namespace etls {
namespace utils {

inline void nameThread(std::string name)
{
    assert(name.size() < 16);
#if defined(_GNU_SOURCE)
    pthread_setname_np(pthread_self(), name.c_str());
#elif defined(APPLE)
    pthread_setname_np(name.c_str());
#endif
}

} // namespace utils
} // namespace etls
} // namespace one

#endif // ONE_ETLS_UTILS_HPP
