#include <unistd.h>

void current_path(char *buf, size_t bufSize)
{
  ssize_t size = readlink("/proc/self/exe", buf, bufSize - 1);
  buf[size == -1 ? 0 : size] = '\0';
}