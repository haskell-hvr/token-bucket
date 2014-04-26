#include <stddef.h>
#include <math.h>
#include <sys/time.h>
#include <stdint.h>

// returns number of microseconds since epoch; returns 0 in case of error
uint64_t hs_token_bucket_get_posix_time_usecs(void)
{
  struct timeval tval = { 0, 0, };

  if (gettimeofday(&tval, NULL))
    return 0;

  return ((((uint64_t)tval.tv_sec) * 1000000) + ((uint64_t) tval.tv_usec));
}
