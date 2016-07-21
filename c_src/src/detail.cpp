/**
 * @file detail.cpp
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.md'
 */

#include "detail.hpp"

#include <asio/ssl/context.hpp>
#include <asio/ssl/rfc2818_verification.hpp>

#include <cassert>
#include <memory>

namespace {

std::unique_ptr<BIO, decltype(&BIO_free)> bufferToBIO(asio::const_buffer buffer)
{
    return {BIO_new_mem_buf(
                const_cast<void *>(asio::buffer_cast<const void *>(buffer)),
                static_cast<int>(asio::buffer_size(buffer))),
        BIO_free};
}

} // namespace

namespace one {
namespace etls {
namespace detail {

WithSSLContext::WithSSLContext(const asio::ssl::context_base::method method,
    const std::string &certPath, const std::string &keyPath,
    std::string rfc2818Hostname)
    : m_context{std::make_shared<asio::ssl::context>(method)}
{
    m_context->set_options(asio::ssl::context::default_workarounds);
    m_context->set_default_verify_paths();
    m_context->set_verify_depth(100);

    if (!certPath.empty()) {
        m_context->use_certificate_chain_file(certPath);
        m_context->use_private_key_file(keyPath, asio::ssl::context::pem);
    }

    std::unique_ptr<X509_VERIFY_PARAM, decltype(&X509_VERIFY_PARAM_free)> param{
        X509_VERIFY_PARAM_new(), X509_VERIFY_PARAM_free};
    if (!param)
        throw std::bad_alloc{};

    X509_VERIFY_PARAM_set_flags(
        param.get(), X509_V_FLAG_CRL_CHECK | X509_V_FLAG_CRL_CHECK_ALL);
    SSL_CTX_set1_param(m_context->native_handle(), param.get());

    X509_STORE_set_flags(
        m_context->native_handle()->cert_store, X509_V_FLAG_ALLOW_PROXY_CERTS);

    m_context->set_verify_callback([
        this, rfc2818Hostname = std::move(rfc2818Hostname)
    ](bool preverify, asio::ssl::verify_context &ctx) mutable {

        if (!preverify &&
            X509_STORE_CTX_get_error(ctx.native_handle()) ==
                X509_V_ERR_UNABLE_TO_GET_CRL)
            preverify = true;

        if (rfc2818Hostname.empty())
            return preverify;

        return asio::ssl::rfc2818_verification{rfc2818Hostname}(preverify, ctx);
    });
}

WithSSLContext::WithSSLContext(std::shared_ptr<asio::ssl::context> context)
    : m_context{std::move(context)}
{
}

void WithSSLContext::addCertificateRevocationList(
    const asio::const_buffer &data)
{
    ERR_clear_error();

    auto bio = bufferToBIO(data);
    if (bio) {
        std::unique_ptr<X509_CRL, decltype(&X509_CRL_free)> crl{
            PEM_read_bio_X509_CRL(bio.get(), nullptr, nullptr, nullptr),
            X509_CRL_free};

        if (crl) {
            if (X509_STORE *store =
                    SSL_CTX_get_cert_store(m_context->native_handle())) {
                if (X509_STORE_add_crl(store, crl.get()) == 1) {
                    return;
                }
            }
        }
    }

    throw std::system_error{
        static_cast<int>(ERR_get_error()), asio::error::get_ssl_category()};
}

void WithSSLContext::addCertificateAuthority(const asio::const_buffer &data)
{
    m_context->add_certificate_authority(data);
}

void WithSSLContext::addChainCertificate(const asio::const_buffer &data)
{
    ERR_clear_error();

    auto bio = bufferToBIO(data);
    if (bio) {
        std::unique_ptr<X509, decltype(&X509_free)> cert{
            PEM_read_bio_X509(bio.get(), nullptr, nullptr, nullptr), X509_free};

        if (cert) {
            if (SSL_CTX_add_extra_chain_cert(
                    m_context->native_handle(), cert.get()) == 1) {
                cert.release();
                return;
            }
        }
    }

    throw std::system_error{
        static_cast<int>(ERR_get_error()), asio::error::get_ssl_category()};
}

void WithSSLContext::setVerifyMode(const asio::ssl::verify_mode mode)
{
    m_context->set_verify_mode(mode);
}

} // namespace detail
} // namespace etls
} // namespace one
