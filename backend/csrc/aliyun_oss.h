#pragma once
#include <stdbool.h>

bool fc_aos_initialize();

void fc_aos_deinitialize();

bool fc_aos_deleteFile(
    const char *accessId,
    const char *accessKeySecret,
    const char *endPoint,
    const char *bucketName,
    const char *objectName);

const char *fc_aos_getUploadUrl(
    const char *accessId,
    const char *accessKeySecret,
    const char *endPoint,
    const char *bucketName,
    const char *objectName);

const char *fc_aos_getDownloadUrl(
    const char *accessId,
    const char *accessKeySecret,
    const char *endPoint,
    const char *bucketName,
    const char *objectName);

char *fc_aos_getFileMeta(
    const char *accessKeyId,
    const char *accessKeySecret,
    const char *endPoint,
    const char *bucketName,
    const char *objectName);
