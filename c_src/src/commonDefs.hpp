/**
 * @file commonDefs.hpp
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONE_ETLS_COMMON_DEFS_HPP
#define ONE_ETLS_COMMON_DEFS_HPP

#include <functional>
#include <string>

#include <system_error>

namespace one {
namespace etls {

/**
 * A function type used to indicate asynchronous operation success.
 */
template <typename... Args> using SuccessFun = std::function<void(Args...)>;

/**
 * A function type used to indicate asynchronous operation error.
 */
using ErrorFun = std::function<void(const std::error_code &)>;

} // namespace etls
} // namespace one

#endif // ONE_ETLS_COMMON_DEFS_HPP
