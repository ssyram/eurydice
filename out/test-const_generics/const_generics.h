/* 
  This file was generated by KaRaMeL <https://github.com/FStarLang/karamel>

  F* version: <unknown>

 */

#ifndef __const_generics_H
#define __const_generics_H

#include "eurydice_glue.h"


#if defined(__cplusplus)
extern "C" {
#endif

static inline void core_num__u32__to_be_bytes(uint32_t x0, uint8_t x1[4U]);

/**
A monomorphic instance of const_generics.serialize
with const generics
- OUT_LEN= 8
*/
void const_generics_serialize_3b(Eurydice_slice re, uint8_t ret[8U]);

void const_generics_main(void);

#define core_panicking_AssertKind_Eq 0
#define core_panicking_AssertKind_Ne 1
#define core_panicking_AssertKind_Match 2

typedef uint8_t core_panicking_AssertKind;

/**
A monomorphic instance of const_generics.Pair
with types uint32_t, uint32_t
with const generics
- $2size_t
- $2size_t
*/
typedef struct const_generics_Pair_4e_s
{
  uint32_t left[2U];
  uint32_t right[2U];
}
const_generics_Pair_4e;

/**
A monomorphic instance of const_generics.Pair
with types uint32_t, uint64_t
with const generics
- $2size_t
- $4size_t
*/
typedef struct const_generics_Pair_a5_s
{
  uint32_t left[2U];
  uint64_t right[4U];
}
const_generics_Pair_a5;

/**
A monomorphic instance of const_generics.Pair
with types uint64_t, uint32_t
with const generics
- $4size_t
- $2size_t
*/
typedef struct const_generics_Pair_87_s
{
  uint64_t left[4U];
  uint32_t right[2U];
}
const_generics_Pair_87;

/**
A monomorphic instance of const_generics.mk_pairs
with types uint32_t, uint64_t
with const generics
- N= 2
- M= 4
*/
const_generics_Pair_4e const_generics_mk_pairs_e0(uint32_t x, uint64_t y);

void const_generics_main1(void);

/**
A monomorphic instance of const_generics.f
with const generics
- FOO= 1
- BAR= 2
*/
bool const_generics_f_e5(uint32_t x, size_t y);

/**
A monomorphic instance of const_generics.f
with const generics
- FOO= 3
- BAR= 4
*/
bool const_generics_f_70(uint32_t x, size_t y);

/**
A monomorphic instance of const_generics.g
with const generics
- BAR= 3
- FOO= 4
*/
bool const_generics_g_70(uint32_t x, size_t y);

void const_generics_main3(void);

extern uint32_t core_clone_impls__core__clone__Clone_for_u32__clone(uint32_t *x0);

extern uint64_t core_clone_impls__core__clone__Clone_for_u64__clone(uint64_t *x0);

extern uint8_t core_clone_impls__core__clone__Clone_for_u8__clone(uint8_t *x0);

#if defined(__cplusplus)
}
#endif

#define __const_generics_H_DEFINED
#endif
