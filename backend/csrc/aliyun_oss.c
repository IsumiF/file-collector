#include "aliyun_oss.h"
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <oss_api.h>
#include <aos_http_io.h>

bool fc_aos_initialize()
{
    return aos_http_io_initialize(NULL, 0) == AOSE_OK;
}

void fc_aos_deinitialize() {
    aos_http_io_deinitialize();
}

static oss_request_options_t *initRequestOptions(
    aos_pool_t *pool,
    const char *accessKeyId,
    const char *accessKeySecret,
    const char *endPoint)
{
    oss_request_options_t *options = oss_request_options_create(pool);
    options->config = oss_config_create(pool);
    aos_str_set(&options->config->endpoint, endPoint);
    aos_str_set(&options->config->access_key_id, accessKeyId);
    aos_str_set(&options->config->access_key_secret, accessKeySecret);
    options->config->is_cname = 0;
    options->ctl = aos_http_controller_create(pool, 0);
    return options;
}

bool fc_aos_deleteFile(
    const char *accessKeyId,
    const char *accessKeySecret,
    const char *endPoint,
    const char *bucketName,
    const char *objectName)
{
    aos_pool_t *pool;
    aos_pool_create(&pool, NULL);

    oss_request_options_t *reqOptions = initRequestOptions(pool, accessKeyId, accessKeySecret, endPoint);
    aos_string_t bucket;
    aos_string_t object;
    aos_table_t *respHeaders = NULL;
    aos_status_t *respStatus = NULL;
    aos_str_set(&bucket, bucketName);
    aos_str_set(&object, objectName);
    respStatus = oss_delete_object(reqOptions, &bucket, &object, &respHeaders);
    bool ret = aos_status_is_ok(respStatus);
    aos_pool_destroy(pool);
    return ret;
}

inline static const char *fc_aos_getSignedUrl(
    const char *accessKeyId,
    const char *accessKeySecret,
    const char *endPoint,
    const char *bucketName,
    const char *objectName,
    http_method_e method)
{
    aos_pool_t *pool;
    aos_pool_create(&pool, NULL);

    oss_request_options_t *ossClientOptions = initRequestOptions(pool, accessKeyId, accessKeySecret, endPoint);

    aos_string_t bucket;
    aos_string_t object;
    aos_str_set(&bucket, bucketName);
    aos_str_set(&object, objectName);
    aos_table_t *headers = NULL;
    headers = aos_table_make(pool, 0);
    apr_time_t now = apr_time_now();
    int64_t expireTime = now / 1000000 + 36000; // expires in one hour
    aos_http_request_t *req = aos_http_request_create(pool);
    req->method = method;

    const char *urlStr =
        oss_gen_signed_url(ossClientOptions, &bucket, &object, expireTime, req);
    size_t urlStrLen = strlen(urlStr);
    char *urlStrRet = malloc((urlStrLen + 1) * sizeof(char));
    memcpy(urlStrRet, urlStr, urlStrLen * sizeof(char));
    urlStrRet[urlStrLen] = '\0';

    aos_pool_destroy(pool);
    return urlStrRet;
}

const char *fc_aos_getUploadUrl(
    const char *accessKeyId,
    const char *accessKeySecret,
    const char *endPoint,
    const char *bucketName,
    const char *objectName)
{
    return fc_aos_getSignedUrl(
        accessKeyId,
        accessKeySecret,
        endPoint,
        bucketName,
        objectName,
        HTTP_PUT);
}

const char *fc_aos_getDownloadUrl(
    const char *accessKeyId,
    const char *accessKeySecret,
    const char *endPoint,
    const char *bucketName,
    const char *objectName)
{
    return fc_aos_getSignedUrl(
        accessKeyId,
        accessKeySecret,
        endPoint,
        bucketName,
        objectName,
        HTTP_GET);
}
