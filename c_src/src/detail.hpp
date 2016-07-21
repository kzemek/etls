/**
 * @file detail.hpp
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.md'
 */

#ifndef ONE_ETLS_DETAIL_HPP
#define ONE_ETLS_DETAIL_HPP

#include <asio/buffer.hpp>
#include <asio/ssl/context.hpp>

#include <memory>

namespace one {
namespace etls {
namespace detail {

/**
 * @c WithSSLContext serves as a base class for classes holding a
 * @c asio::ssl::context member.
 */
class WithSSLContext {
public:
    /**
     * Constructor.
     * Initializes a context.
     * @param method The method used for context initialization.
     * @param certPath Path to the certificate used by the connection.
     * @param keyPath Path to the private key paired with the certificate.
     */
    WithSSLContext(const asio::ssl::context_base::method method,
        const std::string &certPath = "", const std::string &keyPath = "",
        std::string rfc2818Hostname = "");

    /**
     * Constructor.
     * Shares an existing context with @c this . The context can no longer be
     * modified or undefined behaviour will occur.
     * @param other The shared context to use.
     */
    WithSSLContext(std::shared_ptr<asio::ssl::context> context);

    /**
     * Adds a certificate revocation list to the context.
     * @param data PEM-encoded CRL data.
     */
    void addCertificateRevocationList(const asio::const_buffer &data);

    /**
     * Adds a certificate authority to the context.
     * @param data PEM-encoded CA data.
     */
    void addCertificateAuthority(const asio::const_buffer &data);

    /**
     * Adds a certificate to the chain.
     * @param data PEM-encoded certificate.
     */
    void addChainCertificate(const asio::const_buffer &data);

    /**
     * Sets a verification mode on the context.
     * @param mode The verification mode to set.
     */
    virtual void setVerifyMode(const asio::ssl::verify_mode mode);

protected:
    std::shared_ptr<asio::ssl::context> m_context;
};

} // namespace detail
} // namespace etls
} // namespace one

#endif // ONE_ETLS_DETAIL_HPP
