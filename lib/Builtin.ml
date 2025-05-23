module K = struct
  include Krml.Ast
end

(* Helpers to build krml types *)

let vec : K.lident = [ "Eurydice" ], "vec"
let mk_vec (t : K.typ) : K.typ = K.TApp (vec, [ t ])
let slice : K.lident = [ "Eurydice" ], "slice"
let mk_slice (t : K.typ) : K.typ = K.TApp (slice, [ t ])
let range : K.lident = [ "core"; "ops"; "range" ], "Range"
let mk_range (t : K.typ) : K.typ = K.TApp (range, [ t ])
let range_to : K.lident = [ "core"; "ops"; "range" ], "RangeTo"
let mk_range_to (t : K.typ) : K.typ = K.TApp (range_to, [ t ])
let range_from : K.lident = [ "core"; "ops"; "range" ], "RangeFrom"
let mk_range_from (t : K.typ) : K.typ = K.TApp (range_from, [ t ])
let option : K.lident = [ "core"; "option" ], "Option"
let mk_option (t : K.typ) : K.typ = K.TApp (option, [ t ])
let array_copy = [ "Eurydice" ], "array_copy"
let macros = Krml.Idents.LidSet.of_list []

(* Things that could otherwise be emitted as an extern prototype, but for some
   reason ought to be skipped. *)
let skip = Krml.Idents.LidSet.of_list [ [ "Eurydice" ], "assert" ]
let result = [ "core"; "result" ], "Result"
let mk_result t1 t2 = K.TApp (result, [ t1; t2 ])
let nonzero = [ "core"; "num"; "nonzero" ], "NonZero"
let mk_nonzero t = K.TApp (nonzero, [ t ])

(* Internal types *)
let char_t = K.(TInt UInt32)
let int128_t = K.TQualified (["Eurydice"], "int128_t")
let uint128_t = K.TQualified (["Eurydice"], "uint128_t")

(* A record to hold a builtin function with all relevant information for both
   krml and the transpilation phase in AstOfLlbc *)

type builtin = {
  name : K.lident;
  typ : K.typ;
  n_type_args : int;
  cg_args : K.typ list;
  arg_names : string list;
}

let expr_of_builtin { name; typ; cg_args; _ } =
  let typ = List.fold_right (fun t acc -> K.TArrow (t, acc)) cg_args typ in
  K.(with_type typ (EQualified name))

(*
  The real implementations:

Eurydice_int128_t Eurydice_i128_from_bits(uint64_t hi, uint64_t lo) {
  return ((Eurydice_int128_t)hi << 64) | lo;
}
Eurydice_uint128_t Eurydice_u128_from_bits(uint64_t hi, uint64_t lo) {
  return ((Eurydice_uint128_t)hi << 64) | lo;
}
bool Eurydice_i128_eq(Eurydice_int128_t x, Eurydice_int128_t y) {
  return x == y;
}
bool Eurydice_u128_eq(Eurydice_uint128_t x, Eurydice_uint128_t y) {
  return x == y;
}
*)
let i128_from_bits =
  {
    name = [ "Eurydice" ], "i128_from_bits";
    typ = Krml.Helpers.fold_arrow [ Krml.Helpers.uint64; Krml.Helpers.uint64 ] int128_t;
    n_type_args = 0;
    cg_args = [ ];
    arg_names = [ "high"; "low" ];
  }
let u128_from_bits =
  {
    name = [ "Eurydice" ], "u128_from_bits";
    typ = Krml.Helpers.fold_arrow [ Krml.Helpers.uint64; Krml.Helpers.uint64 ] uint128_t;
    n_type_args = 0;
    cg_args = [ ];
    arg_names = [ "high"; "low" ];
  }
let i128_eq =
  {
    name = [ "Eurydice" ], "i128_eq";
    typ = Krml.Helpers.fold_arrow [ int128_t; int128_t ] TBool;
    n_type_args = 0;
    cg_args = [ ];
    arg_names = [ "lhs"; "rhs" ];
  }
let u128_eq =
  {
    name = [ "Eurydice" ], "u128_eq";
    typ = Krml.Helpers.fold_arrow [ uint128_t; uint128_t ] TBool;
    n_type_args = 0;
    cg_args = [ ];
    arg_names = [ "lhs"; "rhs" ];
  }
(*
Eurydice_int128_t Eurydice_i128_add(Eurydice_int128_t lhs, Eurydice_int128_t rhs) {
  return lhs + rhs;
}
Eurydice_uint128_t Eurydice_u128_add(Eurydice_uint128_t lhs, Eurydice_uint128_t rhs) {
  return lhs + rhs;
}
Eurydice_int128_t Eurydice_i128_sub(Eurydice_int128_t lhs, Eurydice_int128_t rhs) {
  return lhs - rhs;
}
Eurydice_uint128_t Eurydice_u128_sub(Eurydice_uint128_t lhs, Eurydice_uint128_t rhs) {
  return lhs - rhs;
}
Eurydice_int128_t Eurydice_i128_mul(Eurydice_int128_t lhs, Eurydice_int128_t rhs) {
  return lhs * rhs;
}
Eurydice_uint128_t Eurydice_u128_mul(Eurydice_uint128_t lhs, Eurydice_uint128_t rhs) {
  return lhs * rhs;
}
Eurydice_int128_t Eurydice_i128_div(Eurydice_int128_t lhs, Eurydice_int128_t rhs) {
  return lhs / rhs;
}
Eurydice_uint128_t Eurydice_u128_div(Eurydice_uint128_t lhs, Eurydice_uint128_t rhs) {
  return lhs / rhs;
}
Eurydice_int128_t Eurydice_i128_mod(Eurydice_int128_t lhs, Eurydice_int128_t rhs) {
  return lhs % rhs;
}
Eurydice_uint128_t Eurydice_u128_mod(Eurydice_uint128_t lhs, Eurydice_uint128_t rhs) {
  return lhs % rhs;
}
Eurydice_int128_t Eurydice_i128_bor(Eurydice_int128_t lhs, Eurydice_int128_t rhs) {
  return lhs | rhs;
}
Eurydice_uint128_t Eurydice_u128_bor(Eurydice_uint128_t lhs, Eurydice_uint128_t rhs) {
  return lhs | rhs;
}
Eurydice_int128_t Eurydice_i128_band(Eurydice_int128_t lhs, Eurydice_int128_t rhs) {
  return lhs & rhs;
}
Eurydice_uint128_t Eurydice_u128_band(Eurydice_uint128_t lhs, Eurydice_uint128_t rhs) {
  return lhs & rhs;
}
Eurydice_int128_t Eurydice_i128_bxor(Eurydice_int128_t lhs, Eurydice_int128_t rhs) {
  return lhs ^ rhs;
}
Eurydice_uint128_t Eurydice_u128_bxor(Eurydice_uint128_t lhs, Eurydice_uint128_t rhs) {
  return lhs ^ rhs;
}
Eurydice_int128_t Eurydice_i128_shl(Eurydice_int128_t lhs, uint32_t rhs) {
  return lhs << rhs;
}
Eurydice_uint128_t Eurydice_u128_shl(Eurydice_uint128_t lhs, uint32_t rhs) {
  return lhs << rhs;
}
Eurydice_int128_t Eurydice_i128_shr(Eurydice_int128_t lhs, uint32_t rhs) {
  return lhs >> rhs;
}
Eurydice_uint128_t Eurydice_u128_shr(Eurydice_uint128_t lhs, uint32_t rhs) {
  return lhs >> rhs;
}
Eurydice_int128_t Eurydice_i128_bnot(Eurydice_int128_t x) {
  return ~x;
}
Eurydice_uint128_t Eurydice_u128_bnot(Eurydice_uint128_t x) {
  return ~x;
}
bool Eurydice_i128_lt(Eurydice_int128_t lhs, Eurydice_int128_t rhs) {
  return lhs < rhs;
}
bool Eurydice_u128_lt(Eurydice_uint128_t lhs, Eurydice_uint128_t rhs) {
  return lhs < rhs;
}
bool Eurydice_i128_gt(Eurydice_int128_t lhs, Eurydice_int128_t rhs) {
  return lhs > rhs;
}
bool Eurydice_u128_gt(Eurydice_uint128_t lhs, Eurydice_uint128_t rhs) {
  return lhs > rhs;
}
bool Eurydice_i128_lte(Eurydice_int128_t lhs, Eurydice_int128_t rhs) {
  return lhs <= rhs;
}
bool Eurydice_u128_lte(Eurydice_uint128_t lhs, Eurydice_uint128_t rhs) {
  return lhs <= rhs;
}
bool Eurydice_i128_gte(Eurydice_int128_t lhs, Eurydice_int128_t rhs) {
  return lhs >= rhs;
}
bool Eurydice_u128_gte(Eurydice_uint128_t lhs, Eurydice_uint128_t rhs) {
  return lhs >= rhs;
}
bool Eurydice_i128_neq(Eurydice_int128_t lhs, Eurydice_int128_t rhs) {
  return lhs != rhs;
}
bool Eurydice_u128_neq(Eurydice_uint128_t lhs, Eurydice_uint128_t rhs) {
  return lhs != rhs;
}
*)
let i128_add =
  {
    name = [ "Eurydice" ], "i128_add";
    typ = Krml.Helpers.fold_arrow [ int128_t; int128_t ] int128_t;
    n_type_args = 0;
    cg_args = [];
    arg_names = [ "lhs"; "rhs" ];
  }

let u128_add =
  {
    name = [ "Eurydice" ], "u128_add";
    typ = Krml.Helpers.fold_arrow [ uint128_t; uint128_t ] uint128_t;
    n_type_args = 0;
    cg_args = [];
    arg_names = [ "lhs"; "rhs" ];
  }

let i128_sub =
  {
    name = [ "Eurydice" ], "i128_sub";
    typ = Krml.Helpers.fold_arrow [ int128_t; int128_t ] int128_t;
    n_type_args = 0;
    cg_args = [];
    arg_names = [ "lhs"; "rhs" ];
  }

let u128_sub =
  {
    name = [ "Eurydice" ], "u128_sub";
    typ = Krml.Helpers.fold_arrow [ uint128_t; uint128_t ] uint128_t;
    n_type_args = 0;
    cg_args = [];
    arg_names = [ "lhs"; "rhs" ];
  }

let i128_mul =
  {
    name = [ "Eurydice" ], "i128_mul";
    typ = Krml.Helpers.fold_arrow [ int128_t; int128_t ] int128_t;
    n_type_args = 0;
    cg_args = [];
    arg_names = [ "lhs"; "rhs" ];
  }

let u128_mul =
  {
    name = [ "Eurydice" ], "u128_mul";
    typ = Krml.Helpers.fold_arrow [ uint128_t; uint128_t ] uint128_t;
    n_type_args = 0;
    cg_args = [];
    arg_names = [ "lhs"; "rhs" ];
  }

let i128_div =
  {
    name = [ "Eurydice" ], "i128_div";
    typ = Krml.Helpers.fold_arrow [ int128_t; int128_t ] int128_t;
    n_type_args = 0;
    cg_args = [];
    arg_names = [ "lhs"; "rhs" ];
  }

let u128_div =
  {
    name = [ "Eurydice" ], "u128_div";
    typ = Krml.Helpers.fold_arrow [ uint128_t; uint128_t ] uint128_t;
    n_type_args = 0;
    cg_args = [];
    arg_names = [ "lhs"; "rhs" ];
  }

let i128_mod =
  {
    name = [ "Eurydice" ], "i128_mod";
    typ = Krml.Helpers.fold_arrow [ int128_t; int128_t ] int128_t;
    n_type_args = 0;
    cg_args = [];
    arg_names = [ "lhs"; "rhs" ];
  }

let u128_mod =
  {
    name = [ "Eurydice" ], "u128_mod";
    typ = Krml.Helpers.fold_arrow [ uint128_t; uint128_t ] uint128_t;
    n_type_args = 0;
    cg_args = [];
    arg_names = [ "lhs"; "rhs" ];
  }

let i128_bor =
  {
    name = [ "Eurydice" ], "i128_bor";
    typ = Krml.Helpers.fold_arrow [ int128_t; int128_t ] int128_t;
    n_type_args = 0;
    cg_args = [];
    arg_names = [ "lhs"; "rhs" ];
  }

let u128_bor =
  {
    name = [ "Eurydice" ], "u128_bor";
    typ = Krml.Helpers.fold_arrow [ uint128_t; uint128_t ] uint128_t;
    n_type_args = 0;
    cg_args = [];
    arg_names = [ "lhs"; "rhs" ];
  }

let i128_band =
  {
    name = [ "Eurydice" ], "i128_band";
    typ = Krml.Helpers.fold_arrow [ int128_t; int128_t ] int128_t;
    n_type_args = 0;
    cg_args = [];
    arg_names = [ "lhs"; "rhs" ];
  }

let u128_band =
  {
    name = [ "Eurydice" ], "u128_band";
    typ = Krml.Helpers.fold_arrow [ uint128_t; uint128_t ] uint128_t;
    n_type_args = 0;
    cg_args = [];
    arg_names = [ "lhs"; "rhs" ];
  }

let i128_bxor =
  {
    name = [ "Eurydice" ], "i128_bxor";
    typ = Krml.Helpers.fold_arrow [ int128_t; int128_t ] int128_t;
    n_type_args = 0;
    cg_args = [];
    arg_names = [ "lhs"; "rhs" ];
  }

let u128_bxor =
  {
    name = [ "Eurydice" ], "u128_bxor";
    typ = Krml.Helpers.fold_arrow [ uint128_t; uint128_t ] uint128_t;
    n_type_args = 0;
    cg_args = [];
    arg_names = [ "lhs"; "rhs" ];
  }

let i128_shl =
  {
    name = [ "Eurydice" ], "i128_shl";
    typ = Krml.Helpers.fold_arrow [ int128_t; TInt UInt32 ] int128_t;
    n_type_args = 0;
    cg_args = [];
    arg_names = [ "lhs"; "rhs" ];
  }

let u128_shl =
  {
    name = [ "Eurydice" ], "u128_shl";
    typ = Krml.Helpers.fold_arrow [ uint128_t; TInt UInt32 ] uint128_t;
    n_type_args = 0;
    cg_args = [];
    arg_names = [ "lhs"; "rhs" ];
  }

let i128_shr =
  {
    name = [ "Eurydice" ], "i128_shr";
    typ = Krml.Helpers.fold_arrow [ int128_t; TInt UInt32 ] int128_t;
    n_type_args = 0;
    cg_args = [];
    arg_names = [ "lhs"; "rhs" ];
  }

let u128_shr =
  {
    name = [ "Eurydice" ], "u128_shr";
    typ = Krml.Helpers.fold_arrow [ uint128_t; TInt UInt32 ] uint128_t;
    n_type_args = 0;
    cg_args = [];
    arg_names = [ "lhs"; "rhs" ];
  }
let i128_bnot = {
  name = [ "Eurydice" ], "i128_bnot";
  typ = Krml.Helpers.fold_arrow [int128_t] int128_t;
  n_type_args = 0;
  cg_args = [];
  arg_names = ["x"];
}

let u128_bnot = {
  name = [ "Eurydice" ], "u128_bnot";
  typ = Krml.Helpers.fold_arrow [uint128_t] uint128_t;
  n_type_args = 0;
  cg_args = [];
  arg_names = ["x"];
}

let i128_lt =
  {
    name = [ "Eurydice" ], "i128_lt";
    typ = Krml.Helpers.fold_arrow [ int128_t; int128_t ] TBool;
    n_type_args = 0;
    cg_args = [];
    arg_names = [ "lhs"; "rhs" ];
  }

let u128_lt =
  {
    name = [ "Eurydice" ], "u128_lt";
    typ = Krml.Helpers.fold_arrow [ uint128_t; uint128_t ] TBool;
    n_type_args = 0;
    cg_args = [];
    arg_names = [ "lhs"; "rhs" ];
  }

let i128_gt =
  {
    name = [ "Eurydice" ], "i128_gt";
    typ = Krml.Helpers.fold_arrow [ int128_t; int128_t ] TBool;
    n_type_args = 0;
    cg_args = [];
    arg_names = [ "lhs"; "rhs" ];
  }

let u128_gt =
  {
    name = [ "Eurydice" ], "u128_gt";
    typ = Krml.Helpers.fold_arrow [ uint128_t; uint128_t ] TBool;
    n_type_args = 0;
    cg_args = [];
    arg_names = [ "lhs"; "rhs" ];
  }
let i128_lte =
  {
    name = [ "Eurydice" ], "i128_lte";
    typ = Krml.Helpers.fold_arrow [ int128_t; int128_t ] TBool;
    n_type_args = 0;
    cg_args = [];
    arg_names = [ "lhs"; "rhs" ];
  }

let u128_lte =
  {
    name = [ "Eurydice" ], "u128_lte";
    typ = Krml.Helpers.fold_arrow [ uint128_t; uint128_t ] TBool;
    n_type_args = 0;
    cg_args = [];
    arg_names = [ "lhs"; "rhs" ];
  }

let i128_gte =
  {
    name = [ "Eurydice" ], "i128_gte";
    typ = Krml.Helpers.fold_arrow [ int128_t; int128_t ] TBool;
    n_type_args = 0;
    cg_args = [];
    arg_names = [ "lhs"; "rhs" ];
  }

let u128_gte =
  {
    name = [ "Eurydice" ], "u128_gte";
    typ = Krml.Helpers.fold_arrow [ uint128_t; uint128_t ] TBool;
    n_type_args = 0;
    cg_args = [];
    arg_names = [ "lhs"; "rhs" ];
  }

let i128_neq =
  {
    name = [ "Eurydice" ], "i128_neq";
    typ = Krml.Helpers.fold_arrow [ int128_t; int128_t ] TBool;
    n_type_args = 0;
    cg_args = [];
    arg_names = [ "lhs"; "rhs" ];
  }

let u128_neq =
  {
    name = [ "Eurydice" ], "u128_neq";
    typ = Krml.Helpers.fold_arrow [ uint128_t; uint128_t ] TBool;
    n_type_args = 0;
    cg_args = [];
    arg_names = [ "lhs"; "rhs" ];
  }

  let i128_addW =
    {
      name = [ "Eurydice" ], "i128_addW";
      typ = Krml.Helpers.fold_arrow [ int128_t; int128_t ] int128_t;
      n_type_args = 0;
      cg_args = [];
      arg_names = [ "lhs"; "rhs" ];
    }

  let u128_addW =
    {
      name = [ "Eurydice" ], "u128_addW";
      typ = Krml.Helpers.fold_arrow [ uint128_t; uint128_t ] uint128_t;
      n_type_args = 0;
      cg_args = [];
      arg_names = [ "lhs"; "rhs" ];
    }

  let i128_subW =
    {
      name = [ "Eurydice" ], "i128_subW";
      typ = Krml.Helpers.fold_arrow [ int128_t; int128_t ] int128_t;
      n_type_args = 0;
      cg_args = [];
      arg_names = [ "lhs"; "rhs" ];
    }

  let u128_subW =
    {
      name = [ "Eurydice" ], "u128_subW";
      typ = Krml.Helpers.fold_arrow [ uint128_t; uint128_t ] uint128_t;
      n_type_args = 0;
      cg_args = [];
      arg_names = [ "lhs"; "rhs" ];
    }

  let i128_divW =
    {
      name = [ "Eurydice" ], "i128_divW";
      typ = Krml.Helpers.fold_arrow [ int128_t; int128_t ] int128_t;
      n_type_args = 0;
      cg_args = [];
      arg_names = [ "lhs"; "rhs" ];
    }

  let u128_divW =
    {
      name = [ "Eurydice" ], "u128_divW";
      typ = Krml.Helpers.fold_arrow [ uint128_t; uint128_t ] uint128_t;
      n_type_args = 0;
      cg_args = [];
      arg_names = [ "lhs"; "rhs" ];
    }

  let i128_multW =
    {
      name = [ "Eurydice" ], "i128_multW";
      typ = Krml.Helpers.fold_arrow [ int128_t; int128_t ] int128_t;
      n_type_args = 0;
      cg_args = [];
      arg_names = [ "lhs"; "rhs" ];
    }

  let u128_multW =
    {
      name = [ "Eurydice" ], "u128_multW";
      typ = Krml.Helpers.fold_arrow [ uint128_t; uint128_t ] uint128_t;
      n_type_args = 0;
      cg_args = [];
      arg_names = [ "lhs"; "rhs" ];
    }

let array_to_slice =
  {
    name = [ "Eurydice" ], "array_to_slice";
    typ = Krml.Helpers.fold_arrow [ TBuf (TBound 0, false) ] (mk_slice (TBound 0));
    n_type_args = 1;
    cg_args = [ TInt SizeT ];
    arg_names = [ "a" ];
  }

let array_to_subslice =
  {
    name = [ "Eurydice" ], "array_to_subslice";
    typ =
      Krml.Helpers.fold_arrow
        [ TBuf (TBound 2, false); mk_range (TInt SizeT) ]
        (mk_slice (TBound 2));
    n_type_args = 3;
    cg_args = [ TInt SizeT ];
    arg_names = [ "a"; "r" ];
  }

let array_to_subslice_to =
  {
    name = [ "Eurydice" ], "array_to_subslice_to";
    typ =
      Krml.Helpers.fold_arrow
        [ TBuf (TBound 2, false); mk_range_to (TInt SizeT) ]
        (mk_slice (TBound 2));
    n_type_args = 3;
    cg_args = [ TInt SizeT ];
    arg_names = [ "a"; "r" ];
  }

let array_to_subslice_from =
  {
    name = [ "Eurydice" ], "array_to_subslice_from";
    typ =
      Krml.Helpers.fold_arrow
        [ TBuf (TBound 2, false); mk_range_from (TInt SizeT) ]
        (mk_slice (TBound 2));
    n_type_args = 3;
    cg_args = [ TInt SizeT ];
    arg_names = [ "a"; "r" ];
  }

let array_repeat =
  {
    name = [ "Eurydice" ], "array_repeat";
    typ = Krml.Helpers.fold_arrow [ TBound 0 ] (TCgArray (TBound 0, 0));
    n_type_args = 1;
    cg_args = [ TInt SizeT ];
    arg_names = [ "init" ];
  }

let iterator : K.lident = [ "core"; "iter"; "traits"; "iterator" ], "Iterator"
let mk_iterator t = K.TApp (iterator, [ t ])

let array_into_iter =
  {
    name = [ "Eurydice" ], "array_into_iter";
    typ = Krml.Helpers.fold_arrow [ TCgArray (TBound 0, 0) ] (mk_iterator (TCgArray (TBound 0, 0)));
    n_type_args = 1;
    cg_args = [ TInt SizeT ];
    arg_names = [ "arr" ];
  }

let step_by : K.lident = [ "core"; "iter"; "adapters"; "step_by" ], "StepBy"
let mk_step_by t = K.TApp (step_by, [ t ])
let mk_range_step_by_iterator t = mk_iterator (mk_step_by t)

(* This is incorrect: the function receives e.g.
   - Range<usize> as its type argument,
   - &StepBy<Range<usize>> for the type of its argument,
   then returns Option<usize> for its return value. Which we can't really type. *)
let range_iterator_step_by =
  {
    name = [ "Eurydice" ], "range_iterator_step_by";
    typ =
      Krml.Helpers.fold_arrow [ mk_range (TBound 0); TInt SizeT ] (mk_step_by (mk_range (TBound 0)));
    n_type_args = 1;
    cg_args = [];
    arg_names = [ "iter" ];
  }

let range_step_by_iterator_next =
  {
    name = [ "Eurydice" ], "range_step_by_iterator_next";
    typ =
      Krml.Helpers.fold_arrow
        [ TBuf (mk_step_by (mk_range (TBound 0)), false) ]
        (mk_option (TBound 0));
    n_type_args = 1;
    cg_args = [];
    arg_names = [ "iter" ];
  }

let slice_index =
  {
    name = [ "Eurydice" ], "slice_index";
    typ = Krml.Helpers.fold_arrow [ mk_slice (TBound 0); TInt SizeT ] (TBound 0);
    n_type_args = 1;
    cg_args = [];
    arg_names = [ "s"; "i" ];
  }

let slice_index_outparam =
  {
    name = [ "Eurydice" ], "slice_index_outparam";
    typ = Krml.Helpers.fold_arrow [ mk_slice (TBound 0); TInt SizeT; TBound 0 ] TUnit;
    n_type_args = 1;
    cg_args = [];
    arg_names = [ "s"; "i"; "dst" ];
  }

let slice_subslice =
  {
    name = [ "Eurydice" ], "slice_subslice";
    typ =
      Krml.Helpers.fold_arrow [ mk_slice (TBound 2); mk_range (TInt SizeT) ] (mk_slice (TBound 2));
    n_type_args = 3;
    cg_args = [];
    arg_names = [ "s"; "r" ];
  }

let slice_subslice_to =
  {
    name = [ "Eurydice" ], "slice_subslice_to";
    typ =
      Krml.Helpers.fold_arrow
        [ mk_slice (TBound 2); mk_range_to (TInt SizeT) ]
        (mk_slice (TBound 2));
    n_type_args = 3;
    cg_args = [];
    arg_names = [ "s"; "r" ];
  }

let slice_subslice_from =
  {
    name = [ "Eurydice" ], "slice_subslice_from";
    typ =
      Krml.Helpers.fold_arrow
        [ mk_slice (TBound 2); mk_range_from (TInt SizeT) ]
        (mk_slice (TBound 2));
    n_type_args = 3;
    cg_args = [];
    arg_names = [ "s"; "r" ];
  }

(* This one comes out naturally of MIR but can't be implemented in C for obvious reasons. *)
let slice_to_array =
  {
    name = [ "Eurydice" ], "slice_to_array";
    typ = Krml.Helpers.fold_arrow [ TBound 2 ] (mk_result (TBound 1) (TBound 0));
    n_type_args = 3;
    cg_args = [];
    arg_names = [ "s" ];
  }

(* This one can be implemented by hand. *)
let slice_to_array2 =
  {
    name = [ "Eurydice" ], "slice_to_array2";
    typ = Krml.Helpers.fold_arrow [ TBuf (mk_result (TBound 1) (TBound 0), false); TBound 2 ] TUnit;
    n_type_args = 3;
    cg_args = [];
    arg_names = [ "dst"; "s" ];
  }

let vec_new =
  {
    name = [ "Eurydice" ], "vec_new";
    typ = Krml.Helpers.fold_arrow [ TUnit ] (mk_vec (TBound 0));
    n_type_args = 1;
    cg_args = [];
    arg_names = [];
  }

let vec_push =
  {
    name = [ "Eurydice" ], "vec_push";
    typ = Krml.Helpers.fold_arrow [ mk_vec (TBound 0); TBound 0 ] TUnit;
    n_type_args = 1;
    cg_args = [];
    arg_names = [ "v"; "x" ];
  }

let vec_len =
  {
    name = [ "Eurydice" ], "vec_len";
    typ = Krml.Helpers.fold_arrow [ mk_vec (TBound 0) ] (TInt SizeT);
    n_type_args = 1;
    cg_args = [];
    arg_names = [ "v" ];
  }

let vec_drop =
  {
    name = [ "Eurydice" ], "vec_drop";
    typ = Krml.Helpers.fold_arrow [ mk_vec (TBound 0) ] TUnit;
    n_type_args = 1;
    cg_args = [];
    arg_names = [ "v" ];
  }

let vec_index =
  {
    name = [ "Eurydice" ], "vec_index";
    typ = Krml.Helpers.fold_arrow [ mk_vec (TBound 0); TInt SizeT ] (TBuf (TBound 0, false));
    n_type_args = 1;
    cg_args = [];
    arg_names = [ "v"; "i" ];
  }

let box_new =
  {
    name = [ "Eurydice" ], "box_new";
    typ = Krml.Helpers.fold_arrow [ TBound 0 ] (TBuf (TBound 0, false));
    n_type_args = 1;
    cg_args = [];
    arg_names = [ "v" ];
  }

let box_new_array =
  {
    name = [ "Eurydice" ], "box_new_array";
    typ = Krml.Helpers.fold_arrow [ TCgArray (TBound 0, 0) ] (TBuf (TBound 0, false));
    n_type_args = 1;
    cg_args = [ TInt SizeT ];
    arg_names = [ "v" ];
  }

let replace =
  {
    name = [ "Eurydice" ], "replace";
    typ = Krml.Helpers.fold_arrow [ TBuf (TBound 0, false); TBound 0 ] (TBound 0);
    n_type_args = 1;
    cg_args = [];
    arg_names = [ "v"; "x" ];
  }

let dst = [ "Eurydice" ], "dst"
let derefed_slice = [ "Eurydice" ], "derefed_slice"

let dst_def =
  K.DType
    ( dst,
      [],
      0,
      1,
      Flat
        [
          Some "ptr", (TBuf (TBound 0, false), false);
          Some "len", (TInt SizeT, false);
          (* a number of elements, just like slices *)
        ] )

let mk_dst t : K.typ = TApp (dst, [ t ])

(* Gotta use a helper because the definition of Eurydice_slice is opaque (historical mistake?). *)
let slice_of_dst =
  {
    name = [ "Eurydice" ], "slice_of_dst";
    typ =
      Krml.Helpers.fold_arrow
        [ TBuf (TApp (derefed_slice, [ TBound 0 ]), false); TInt SizeT ]
        (mk_slice (TBound 0));
    n_type_args = 1;
    cg_args = [];
    arg_names = [ "ptr"; "len" ];
  }

(* Gotta use a helper because the definition of Eurydice_slice is opaque (historical mistake?). *)
let slice_of_boxed_array =
  {
    name = [ "Eurydice" ], "slice_of_boxed_array";
    typ = Krml.Helpers.fold_arrow [ TBuf (TBound 0, false); TInt SizeT ] (mk_slice (TBound 0));
    n_type_args = 1;
    cg_args = [];
    arg_names = [ "ptr"; "len" ];
  }

(* Take the type of the ptr field *)
let dst_new ~ptr ~len t =
  let open K in
  with_type (mk_dst t) (EFlat [ Some "ptr", ptr; Some "len", len ])

(* pointer, value *)
let bitand_pv_u8 =
  {
    name = [ "Eurydice" ], "bitand_pv_u8";
    typ = Krml.Helpers.fold_arrow [ TBuf (TInt UInt8, false); TInt UInt8 ] (TInt UInt8);
    n_type_args = 0;
    cg_args = [];
    arg_names = [ "x"; "y" ];
  }

let shr_pv_u8 =
  {
    name = [ "Eurydice" ], "shr_pv_u8";
    typ = Krml.Helpers.fold_arrow [ TBuf (TInt UInt8, false); TInt Int32 ] (TInt UInt8);
    n_type_args = 0;
    cg_args = [];
    arg_names = [ "x"; "y" ];
  }

let min_u32 =
  {
    name = [ "Eurydice" ], "min_u32";
    typ = Krml.Helpers.fold_arrow [ TInt UInt32; TInt UInt32 ] (TInt UInt32);
    n_type_args = 0;
    cg_args = [];
    arg_names = [ "x"; "y" ];
  }

(* Not fully general *)
let static_assert, static_assert_ref =
  let name = [ "Eurydice" ], "assert" in
  let typ = Krml.Helpers.fold_arrow [ TBool; Krml.Checker.c_string ] TUnit in
  ( K.DExternal (None, [ Krml.Common.Private; Macro ], 0, 0, name, typ, [ "test"; "msg" ]),
    K.(with_type typ (EQualified name)) )

let unwrap : K.decl =
  let open Krml in
  let open Ast in
  let lid =
    [ "core"; "result"; "{core::result::Result<T, E>[TraitClause@0, TraitClause@1]}" ], "unwrap"
  in
  let t_T = TBound 1 in
  let t_E = TBound 0 in
  let t_result = mk_result t_T t_E in
  let binders = [ Helpers.fresh_binder "self" t_result ] in
  DFunction
    ( None,
      [ Private ],
      0,
      2,
      t_T,
      lid,
      binders,
      with_type t_T
        (EMatch
           ( Unchecked,
             with_type t_result (EBound 0),
             [
               ( [ Helpers.fresh_binder "f0" t_T ],
                 with_type t_result (PCons ("Ok", [ with_type t_T (PBound 0) ])),
                 with_type t_T (EBound 0) );
               [], with_type t_result PWild, with_type t_T (EAbort (Some t_T, Some "unwrap not Ok"));
             ] )) )

let nonzero_def = K.DType (nonzero, [], 0, 1, Abbrev (TBound 0))

(* -------------------------------------------------------------------------- *)

type usage = Used | Unused

let replacements = List.map (fun decl -> K.lid_of_decl decl, (decl, ref Unused)) [ unwrap ]

let files =
  [
    Krml.Builtin.lowstar_ignore;
    (let externals =
       List.map
         (fun { name; typ; cg_args; n_type_args; arg_names } ->
           let typ = Krml.Helpers.fold_arrow cg_args typ in
           let flags =
             (* FIXME: calls to this are generated *after* the reachability
                analysis during one of the desugaring phases, so there's no good
                way right now to prevent it from being eliminated *)
             if name = ([ "Eurydice" ], "slice_to_array2") then
               []
             else
               [ Krml.Common.Private ]
           in
           K.DExternal (None, flags, List.length cg_args, n_type_args, name, typ, arg_names))
         [
          i128_from_bits;
          u128_from_bits;
          i128_eq;
          u128_eq;
          i128_add;
          u128_add;
          i128_sub;
          u128_sub;
          i128_mul;
          u128_mul;
          i128_div;
          u128_div;
          i128_mod;
          u128_mod;
          i128_bor;
          u128_bor;
          i128_band;
          u128_band;
          i128_bxor;
          u128_bxor;
          i128_shl;
          u128_shl;
          i128_shr;
          u128_shr;
          i128_bnot;
          u128_bnot;
          i128_lt;
          u128_lt;
          i128_gt;
          u128_gt;
          i128_lte;
          u128_lte;
          i128_gte;
          u128_gte;
          i128_neq;
          u128_neq;
          i128_addW;
          u128_addW;
          i128_subW;
          u128_subW;
          i128_divW;
          u128_divW;
          i128_multW;
          u128_multW;
           array_to_slice;
           array_to_subslice;
           array_to_subslice_to;
           array_to_subslice_from;
           array_repeat;
           array_into_iter;
           slice_index;
           slice_index_outparam;
           slice_subslice;
           slice_subslice_to;
           slice_subslice_from;
           slice_to_array;
           slice_to_array2;
           range_iterator_step_by;
           range_step_by_iterator_next;
           vec_push;
           vec_new;
           vec_len;
           vec_drop;
           vec_index;
           box_new;
           box_new_array;
           replace;
           slice_of_dst;
           slice_of_boxed_array;
           bitand_pv_u8;
           shr_pv_u8;
           min_u32;
         ]
       @ [ nonzero_def; static_assert; dst_def ]
     in
     "Eurydice", externals);
  ]

let adjust (f, decls) =
  ( f,
    List.map
      (function
        | Krml.Ast.DExternal (_, _, _, _, (([ "core"; "num"; mid ], "BITS") as lid), _, _)
          when Krml.KString.starts_with mid "{u32" ->
            Krml.Ast.DGlobal ([], lid, 0, Krml.Helpers.uint32, Krml.Helpers.mk_uint32 32)
        | d -> (
            try
              let d, seen = List.assoc (K.lid_of_decl d) replacements in
              seen := Used;
              d
            with Not_found -> d))
      decls )

let check () =
  List.iter
    (fun (lid, (_, seen)) ->
      if !seen = Unused then
        let open Krml in
        KPrint.bprintf "Unused replacement: %a\n" PrintAst.Ops.plid lid)
    replacements
