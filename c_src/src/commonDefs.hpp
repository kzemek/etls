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

namespace one {
namespace etls {

template <typename... Args> using SuccessFun = std::function<void(Args...)>;

using ErrorFun = std::function<void(std::string)>;

} // namespace etls
} // namespace one

#endif // ONE_ETLS_COMMON_DEFS_HPP
