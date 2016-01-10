#include <erl_nif.h>
#include <openssl/ssl.h>

struct priv_data_t {
    SSL_CTX *ssl_ctx;
    ErlNifResourceType *ssl_res_t;
};

static void ssl_res_t_dtor(ErlNifEnv *env, void *obj)
{
    if (obj)
        SSL_free((SSL *)obj);
}

static struct priv_data_t *get_priv_data(ErlNifEnv *env)
{
    return (struct priv_data_t *)enif_priv_data(env);
}

static SSL_CTX *get_ssl_ctx(ErlNifEnv *env)
{
    return get_priv_data(env)->ssl_ctx;
}

static ErlNifResourceType *get_ssl_res_t(ErlNifEnv *env)
{
    return get_priv_data(env)->ssl_res_t;
}

static int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    struct priv_data_t *priv =
        (struct priv_data_t *)enif_alloc(sizeof(struct priv_data_t));
    if (!priv_data)
        return 1;

    priv->ssl_res_t = enif_open_resource_type(
        env, 0, "SSL*", ssl_res_t_dtor, ERL_NIF_RT_CREATE, 0);
    if (!priv->ssl_res_t) {
        enif_free(priv);
        return 2;
    }

    priv->ssl_ctx = SSL_CTX_new(TLS_method());
    if (!priv->ssl_ctx) {
        enif_free(priv);
        return 3;
    }

    *priv_data = priv;
    return 0;
}

static void unload(ErlNifEnv *env, void *priv_data)
{
    SSL_CTX_free(((struct priv_data_t *)priv_data)->ssl_ctx);
    enif_free(priv_data);
}

static ERL_NIF_TERM etls_new(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    SSL **ssl_res =
        (SSL **)enif_alloc_resource(get_ssl_res_t(env), sizeof(SSL *));

    if (!ssl_res)
        return enif_raise_exception(env, enif_make_atom(env, "out_of_memory"));

    *ssl_res = SSL_new(get_ssl_ctx(env));
    if (!*ssl_res) {
        enif_release_resource(ssl_res);
        return enif_raise_exception(env, enif_make_atom(env, "out_of_memory"));
    }

    ERL_NIF_TERM res = enif_make_resource(env, ssl_res);
    enif_release_resource(ssl_res);
    return res;
}

static ERL_NIF_TERM etls_get_error(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    SSL *ssl;
    if (!enif_get_resource(env, argv[0], get_ssl_res_t(env), (void **)&ssl))
        return enif_make_badarg(env);

    int ret_code;
    if (!enif_get_int(env, argv[1], &ret_code))
        return enif_make_badarg(env);

    int ssl_err = SSL_get_error(ssl, ret_code);
    switch (ssl_err) {
        case SSL_ERROR_NONE:
            return enif_make_atom(env, "SSL_ERROR_NONE");
        case SSL_ERROR_SSL:
            return enif_make_atom(env, "SSL_ERROR_SSL");
        case SSL_ERROR_WANT_READ:
            return enif_make_atom(env, "SSL_ERROR_WANT_READ");
        case SSL_ERROR_WANT_WRITE:
            return enif_make_atom(env, "SSL_ERROR_WANT_WRITE");
        case SSL_ERROR_SYSCALL:
            return enif_make_atom(env, "SSL_ERROR_SYSCALL");
        case SSL_ERROR_ZERO_RETURN:
            return enif_make_atom(env, "SSL_ERROR_ZERO_RETURN");
        case SSL_ERROR_WANT_CONNECT:
            return enif_make_atom(env, "SSL_ERROR_WANT_CONNECT");
        case SSL_ERROR_WANT_ACCEPT:
            return enif_make_atom(env, "SSL_ERROR_WANT_ACCEPT");
        case SSL_ERROR_WANT_CHANNEL_ID_LOOKUP:
            return enif_make_atom(env, "SSL_ERROR_WANT_CHANNEL_ID_LOOKUP");
        case SSL_ERROR_PENDING_SESSION:
            return enif_make_atom(env, "SSL_ERROR_PENDING_SESSION");
        case SSL_ERROR_PENDING_CERTIFICATE:
            return enif_make_atom(env, "SSL_ERROR_PENDING_CERTIFICATE");
        case SSL_ERROR_WANT_PRIVATE_KEY_OPERATION:
            return enif_make_atom(env, "SSL_ERROR_WANT_PRIVATE_KEY_OPERATION");
        default:
            return enif_make_tuple2(env,
                enif_make_atom(env, "unknown_openssl_error"),
                enif_make_int(env, ssl_err));
    }
}

static ErlNifFunc nif_funcs[] = {
    {"new", 0, etls_new}, {"get_error", 2, etls_get_error}};

ERL_NIF_INIT(etls_ssl, nif_funcs, load, 0, 0, unload)
