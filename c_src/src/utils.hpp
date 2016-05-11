/**
 * @file utils.hpp
 * @author Konrad Zemek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
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

#ifdef _GNU_SOURCE
inline void nameThread(std::thread &t, std::string name)
{
    assert(name.size() < 16);
    pthread_setname_np(t.native_handle(), name.c_str());
}
#else
inline void nameThread(std::thread, std::string) {}
#endif

} // namespace utils
} // namespace etls
} // namespace one

#endif // ONE_ETLS_UTILS_HPP
