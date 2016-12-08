#include "cas.h"

bool cas32(uint32_t *ptr, uint32_t expected, uint32_t desired) {
  //bool __atomic_compare_exchange_n (type *ptr, type *expected, type desired, bool weak, int success_memorder, int failure_memorder)
  return __atomic_compare_exchange_n( ptr, &expected, desired, false, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST );
}

bool cas64(uint64_t *ptr, uint64_t expected, uint64_t desired) {
  //bool __atomic_compare_exchange_n (type *ptr, type *expected, type desired, bool weak, int success_memorder, int failure_memorder)
  return __atomic_compare_exchange_n( ptr, &expected, desired, false, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST );
}

bool cas128(__int128 *ptr, __int128 expected, __int128 desired) {
  //bool __atomic_compare_exchange_n (type *ptr, type *expected, type desired, bool weak, int success_memorder, int failure_memorder)
  return __atomic_compare_exchange_n( ptr, &expected, desired, false, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST );
}

