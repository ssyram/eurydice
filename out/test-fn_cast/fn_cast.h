/* 
  This file was generated by KaRaMeL <https://github.com/FStarLang/karamel>

  F* version: <unknown>

 */

#ifndef __fn_cast_H
#define __fn_cast_H

#include "eurydice_glue.h"


#if defined(__cplusplus)
extern "C" {
#endif

#define core_panicking_AssertKind_Eq 0
#define core_panicking_AssertKind_Ne 1
#define core_panicking_AssertKind_Match 2

typedef uint8_t core_panicking_AssertKind;

/**
A monomorphic instance of fn_cast.applies
with types int32_t, int32_t

*/
int32_t *fn_cast_applies_99(int32_t *(*f)(int32_t *x0), int32_t *arg);

/**
A monomorphic instance of fn_cast.id_ref
with types int32_t

*/
int32_t *fn_cast_id_ref_a8(int32_t *x);

void fn_cast_main(void);

#if defined(__cplusplus)
}
#endif

#define __fn_cast_H_DEFINED
#endif
