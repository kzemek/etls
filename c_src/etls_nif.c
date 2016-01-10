#include <erl_nif.h>
#include <openssl/ssl.h>
#include <openssl/bio.h>

struct priv_data_t {
    SSL_CTX *ssl_ctx;
    ErlNifResourceType *ssl_res_t;
    ErlNifResourceType *bio_res_t;
};

static void ssl_res_t_dtor(ErlNifEnv *env, void *obj)
{
    if (obj)
        SSL_free(*(SSL **)obj);
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

static ErlNifResourceType *get_bio_res_t(ErlNifEnv *env)
{
    return get_priv_data(env)->bio_res_t;
}

static ERL_NIF_TERM oom(ErlNifEnv *env)
{
    return enif_raise_exception(env, enif_make_atom(env, "out_of_memory"));
}

static SSL *get_ssl(ErlNifEnv *env, ERL_NIF_TERM term)
{
    SSL **ssl;
    if (!enif_get_resource(env, term, get_ssl_res_t(env), (void **)&ssl))
        return 0;

    return *ssl;
}

static BIO *get_bio(ErlNifEnv *env, ERL_NIF_TERM term)
{
    BIO **bio;
    if (!enif_get_resource(env, term, get_bio_res_t(env), (void **)&bio))
        return 0;

    return *bio;
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

    priv->bio_res_t =
        enif_open_resource_type(env, 0, "BIO*", 0, ERL_NIF_RT_CREATE, 0);
    if (!priv->bio_res_t) {
        enif_free(priv);
        return 3;
    }

    priv->ssl_ctx = SSL_CTX_new(TLS_method());
    if (!priv->ssl_ctx) {
        enif_free(priv);
        return 4;
    }

    *priv_data = priv;
    return 0;
}

static void unload(ErlNifEnv *env, void *priv_data)
{
    SSL_CTX_free(((struct priv_data_t *)priv_data)->ssl_ctx);
    enif_free(priv_data);
}

static ERL_NIF_TERM bio_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    BIO **bio_res =
        (BIO **)enif_alloc_resource(get_bio_res_t(env), sizeof(BIO *));

    if (!bio_res)
        return oom(env);

    *bio_res = BIO_new(BIO_s_mem());
    if (!*bio_res) {
        enif_release_resource(bio_res);
        return oom(env);
    }

    ERL_NIF_TERM res = enif_make_resource(env, bio_res);
    enif_release_resource(bio_res);
    return res;
}

static ERL_NIF_TERM bio_read(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    BIO *bio = get_bio(env, argv[0]);
    if (!bio)
        return enif_make_badarg(env);

    size_t pending = BIO_pending(bio);
    if (pending == 0) {
        ERL_NIF_TERM binterm;
        enif_make_new_binary(env, 0, &binterm);
        return binterm;
    }

    ErlNifBinary bin;
    if (!enif_alloc_binary(pending, &bin))
        return oom(env);

    int read = BIO_read(bio, bin.data, pending);
    if (read >= 0) {
        if (!enif_realloc_binary(&bin, read))
            return oom(env);

        return enif_make_binary(env, &bin);
    }

    enif_release_binary(&bin);
    return enif_make_int(env, read);
}

static ERL_NIF_TERM bio_write(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    BIO *bio = get_bio(env, argv[0]);
    if (!bio)
        return enif_make_badarg(env);

    ErlNifBinary bin;
    if (!enif_inspect_binary(env, argv[1], &bin))
        return enif_make_badarg(env);

    return enif_make_int(env, BIO_write(bio, bin.data, bin.size));
}

static ERL_NIF_TERM ssl_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    SSL **ssl_res =
        (SSL **)enif_alloc_resource(get_ssl_res_t(env), sizeof(SSL *));

    if (!ssl_res)
        return oom(env);

    *ssl_res = SSL_new(get_ssl_ctx(env));
    if (!*ssl_res) {
        enif_release_resource(ssl_res);
        return oom(env);
    }

    ERL_NIF_TERM res = enif_make_resource(env, ssl_res);
    enif_release_resource(ssl_res);
    return res;
}

static ERL_NIF_TERM ssl_get_error(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    SSL *ssl = get_ssl(env, argv[0]);
    if (!ssl)
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

static ERL_NIF_TERM ssl_set_bio(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    SSL *ssl = get_ssl(env, argv[0]);
    if (!ssl)
        return enif_make_badarg(env);

    BIO *rbio = get_bio(env, argv[1]);
    if (!rbio)
        return enif_make_badarg(env);

    BIO *wbio = get_bio(env, argv[2]);
    if (!wbio)
        return enif_make_badarg(env);

    SSL_set_bio(ssl, rbio, wbio);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM ssl_set_connect_state(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    SSL *ssl = get_ssl(env, argv[0]);
    if (!ssl)
        return enif_make_badarg(env);

    SSL_set_connect_state(ssl);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM ssl_do_handshake(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    SSL *ssl = get_ssl(env, argv[0]);
    if (!ssl)
        return enif_make_badarg(env);

    return enif_make_int(env, SSL_do_handshake(ssl));
}

static ERL_NIF_TERM ssl_write(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    SSL *ssl = get_ssl(env, argv[0]);
    if (!ssl)
        return enif_make_badarg(env);

    ErlNifBinary bin;
    if (!enif_inspect_binary(env, argv[1], &bin))
        return enif_make_badarg(env);

    return enif_make_int(env, SSL_write(ssl, bin.data, bin.size));
}

static ErlNifFunc nif_funcs[] = {{"bio_new", 0, bio_new},
    {"bio_read", 1, bio_read}, {"bio_write", 2, bio_write},
    {"ssl_new", 0, ssl_new}, {"ssl_get_error", 2, ssl_get_error},
    {"ssl_set_bio", 3, ssl_set_bio},
    {"ssl_set_connect_state", 1, ssl_set_connect_state},
    {"ssl_do_handshake", 1, ssl_do_handshake, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"ssl_write", 2, ssl_write, ERL_NIF_DIRTY_JOB_CPU_BOUND}};

ERL_NIF_INIT(etls_nif, nif_funcs, load, 0, 0, unload)
