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

template <class T, class S, typename Res, typename... Args1, typename... Args2>
void notifying(SuccessFun<Res> &&success, ErrorFun &&error,
    Res (T::*method)(Args1...), const S &self, Args2 &&... args)
{
    try {
        Res &&result = ((*self).*method)(self, std::forward<Args2>(args)...);
        success(std::forward<Res>(result));
    }
    catch (const std::exception &e) {
        error(e.what());
    }
}

template <class T, class S, typename Res, typename... Args1, typename... Args2>
void notifying(SuccessFun<> &&success, ErrorFun &&error,
    Res (T::*method)(Args1...), const S &self, Args2 &&... args)
{
    try {
        ((*self).*method)(self, std::forward<Args2>(args)...);
        success();
    }
    catch (const std::exception &e) {
        error(e.what());
    }
}

} // namespace etls
} // namespace one

#endif // ONE_ETLS_COMMON_DEFS_HPP
