#include <stdlib.h>

#ifdef __linux__
#include <unistd.h>
#elif _WIN32
#include <libloaderapi.h>
#endif

void current_path(char *buf, size_t bufSize)
{
#ifdef __linux__
  ssize_t size = readlink("/proc/self/exe", buf, bufSize - 1);
  buf[size == -1 ? 0 : size] = '\0';
#elif _WIN32

#else
  buf[0] = '\0';
#endif
}