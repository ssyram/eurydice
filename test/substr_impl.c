#include "substr.h"

size_t core_str__str__len(Eurydice_dst_ref_shared_65 x0) {
  return x0.meta;
}

Eurydice_dst_ref_shared_65
core_str_traits__core__ops__index__Index_I__Clause1_Output__for_str__index_02(
  Eurydice_dst_ref_shared_65 x0,
  core_ops_range_RangeFrom_08 x1
) {
  return (Eurydice_dst_ref_shared_65) {
    .ptr = x0.ptr + x1.start,
    .meta = x0.meta - x1.start
  };
}
