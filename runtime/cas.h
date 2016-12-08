#ifndef cas_h
#define cas_h
#include <stdint.h>
#include <stdbool.h>

bool cas32(uint32_t *ptr, uint32_t expected, uint32_t desired);

bool cas64(uint64_t *ptr, uint64_t expected, uint64_t desired);

bool cas128(__int128 *ptr, __int128 expected, __int128 desired);

#endif
