/**
 * @file callback.hpp
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
 * A callback class for signalling either an error or a success.
 */
template <typename... Args> class Callback {
public:
    template <typename SF, typename EF>
    Callback(SF &&successFun, EF &&errorFun)
        : m_successFun(std::forward<SF>(successFun))
        , m_errorFun(std::forward<EF>(errorFun))
    {
    }

    Callback(Callback &&) = default;
    Callback(const Callback &) = delete;
    Callback &operator=(Callback &&) = default;
    Callback &operator=(const Callback &) = delete;

    void operator()() const { m_successFun(); }

    template <typename... Args2,
        typename std::enable_if<
            std::is_convertible<Args2..., Args...>::value>::type * = nullptr>
    void operator()(Args2 &&... args) const
    {
        m_successFun(std::forward<Args2>(args)...);
    }

    void operator()(const std::error_code &ec) const { m_errorFun(ec); }

private:
    std::function<void(Args...)> m_successFun;
    std::function<void(const std::error_code)> m_errorFun;
};

} // namespace etls
} // namespace one

#endif // ONE_ETLS_COMMON_DEFS_HPP
