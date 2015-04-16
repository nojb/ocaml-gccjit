(* The MIT License (MIT)

   Copyright (c) 2015 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

exception Error of string

module B = Gccjit_bindings.Bindings (Gccjit_types_generated) (Gccjit_stubs_generated)

open Gccjit_bindings
open B

type context = gcc_jit_context
type result = gcc_jit_result
type location = [ `Location of gcc_jit_location ]
type param = [ `Param of gcc_jit_param ]
type lvalue = [ `Lvalue of gcc_jit_lvalue | param ]
type rvalue = [ `Rvalue of gcc_jit_rvalue | lvalue ]
type field = [ `Field of gcc_jit_field ]
type struct_ = [ `Struct of gcc_jit_struct ]
type type_ = [ `Type of gcc_jit_type | struct_ ]
type function_ = [ `Function of gcc_jit_function ]
type block = [ `Block of gcc_jit_block ]
type object_ = [ location | type_ | field | function_ | block | rvalue ]

let null_loc = `Location (Ctypes.(coerce (ptr void) gcc_jit_location null))

type unary_op =
  | Negate
  | Bitwise_negate
  | Logical_negate

type binary_op =
  | Plus
  | Minus
  | Mult
  | Divide
  | Modulo
  | Bitwise_and
  | Bitwise_xor
  | Bitwise_or
  | Logical_and
  | Logical_or

type comparison =
  | Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge

type function_kind =
  | Exported
  | Internal
  | Imported
  | Always_inline

type global_kind =
  | Global_exported
  | Global_internal
  | Global_imported

type _ context_option =
  | Progname : string context_option
  | Optimization_level : int context_option
  | Debuginfo : bool context_option
  | Dump_initial_tree : bool context_option
  | Dump_initial_gimple : bool context_option
  | Dump_generated_code : bool context_option
  | Dump_summary : bool context_option
  | Dump_everything : bool context_option
  | Selfcheck_gc : bool context_option
  | Keep_intermediates : bool context_option

type output_kind =
  | Assembler
  | Object_file
  | Dynamic_library
  | Executable

type type_kind =
  | Void
  | Void_ptr
  | Bool
  | Char
  | Signed_char
  | Unsigned_char
  | Short
  | Unsigned_short
  | Int
  | Unsigned_int
  | Long
  | Unsigned_long
  | Long_long
  | Unsigned_long_long
  | Float
  | Double
  | Long_double
  | Const_char_ptr
  | Size_t
  | File_ptr
  | Complex_float
  | Complex_double
  | Complex_long_double

let function_kind = function
  | Exported -> GCC_JIT_FUNCTION_EXPORTED
  | Internal -> GCC_JIT_FUNCTION_INTERNAL
  | Imported -> GCC_JIT_FUNCTION_IMPORTED
  | Always_inline -> GCC_JIT_FUNCTION_ALWAYS_INLINE

let lvalue' = function
  | `Lvalue lval -> lval
  | `Param param ->
      gcc_jit_param_as_lvalue param

let rvalue' = function
  | `Lvalue lval ->
      gcc_jit_lvalue_as_rvalue lval
  | `Rvalue rval -> rval
  | `Param param ->
      gcc_jit_param_as_rvalue param

let type_kind = function
  | Void -> GCC_JIT_TYPE_VOID
  | Void_ptr -> GCC_JIT_TYPE_VOID_PTR
  | Bool -> GCC_JIT_TYPE_BOOL
  | Char -> GCC_JIT_TYPE_CHAR
  | Signed_char -> GCC_JIT_TYPE_SIGNED_CHAR
  | Unsigned_char -> GCC_JIT_TYPE_UNSIGNED_CHAR
  | Short -> GCC_JIT_TYPE_SHORT
  | Unsigned_short -> GCC_JIT_TYPE_UNSIGNED_SHORT
  | Int -> GCC_JIT_TYPE_INT
  | Unsigned_int -> GCC_JIT_TYPE_UNSIGNED_INT
  | Long -> GCC_JIT_TYPE_LONG
  | Unsigned_long -> GCC_JIT_TYPE_UNSIGNED_LONG
  | Long_long -> GCC_JIT_TYPE_LONG_LONG
  | Unsigned_long_long -> GCC_JIT_TYPE_UNSIGNED_LONG_LONG
  | Float -> GCC_JIT_TYPE_FLOAT
  | Double -> GCC_JIT_TYPE_DOUBLE
  | Long_double -> GCC_JIT_TYPE_LONG_DOUBLE
  | Const_char_ptr -> GCC_JIT_TYPE_CONST_CHAR_PTR
  | Size_t -> GCC_JIT_TYPE_SIZE_T
  | File_ptr -> GCC_JIT_TYPE_FILE_PTR
  | Complex_float -> GCC_JIT_TYPE_COMPLEX_FLOAT
  | Complex_double -> GCC_JIT_TYPE_COMPLEX_DOUBLE
  | Complex_long_double -> GCC_JIT_TYPE_COMPLEX_LONG_DOUBLE

let object' = function
  | `Location loc -> gcc_jit_location_as_object loc
  | `Type typ -> gcc_jit_type_as_object typ
  | `Struct struc -> gcc_jit_type_as_object (gcc_jit_struct_as_type struc)
  | `Field field -> gcc_jit_field_as_object field
  | `Function fn -> gcc_jit_function_as_object fn
  | `Block block -> gcc_jit_block_as_object block
  | `Rvalue rvalue -> gcc_jit_rvalue_as_object rvalue
  | `Lvalue lvalue -> gcc_jit_lvalue_as_object lvalue
  | `Param param -> gcc_jit_param_as_object param

let binary_op = function
  | Plus -> GCC_JIT_BINARY_OP_PLUS
  | Minus -> GCC_JIT_BINARY_OP_MINUS
  | Mult -> GCC_JIT_BINARY_OP_MULT
  | Divide -> GCC_JIT_BINARY_OP_DIVIDE
  | Modulo -> GCC_JIT_BINARY_OP_MODULO
  | Bitwise_and -> GCC_JIT_BINARY_OP_BITWISE_AND
  | Bitwise_xor -> GCC_JIT_BINARY_OP_BITWISE_XOR
  | Bitwise_or -> GCC_JIT_BINARY_OP_BITWISE_OR
  | Logical_and -> GCC_JIT_BINARY_OP_LOGICAL_AND
  | Logical_or -> GCC_JIT_BINARY_OP_LOGICAL_OR

let comparison = function
  | Eq -> GCC_JIT_COMPARISON_EQ
  | Ne -> GCC_JIT_COMPARISON_NE
  | Lt -> GCC_JIT_COMPARISON_LT
  | Le -> GCC_JIT_COMPARISON_LE
  | Gt -> GCC_JIT_COMPARISON_GT
  | Ge -> GCC_JIT_COMPARISON_GE

let unary_op = function
  | Negate -> GCC_JIT_UNARY_OP_MINUS
  | Bitwise_negate -> GCC_JIT_UNARY_OP_BITWISE_NEGATE
  | Logical_negate -> GCC_JIT_UNARY_OP_LOGICAL_NEGATE

let global_kind = function
  | Global_exported -> GCC_JIT_GLOBAL_EXPORTED
  | Global_imported -> GCC_JIT_GLOBAL_IMPORTED
  | Global_internal -> GCC_JIT_GLOBAL_INTERNAL

let typ' = function
  | `Struct struc ->
      gcc_jit_struct_as_type struc
  | `Type typ ->
      typ

let loc' (`Location loc) = loc

let wrap1 ctx f x1 =
  let y = f x1 in
  match gcc_jit_context_get_first_error ctx with
  | None -> y
  | Some err -> raise (Error err)

let wrap2 ctx f x1 x2 =
  let y = f x1 x2 in
  match gcc_jit_context_get_first_error ctx with
  | None -> y
  | Some err -> raise (Error err)

let wrap3 ctx f x1 x2 x3 =
  let y = f x1 x2 x3 in
  match gcc_jit_context_get_first_error ctx with
  | None -> y
  | Some err -> raise (Error err)

let wrap4 ctx f x1 x2 x3 x4 =
  let y = f x1 x2 x3 x4 in
  match gcc_jit_context_get_first_error ctx with
  | None -> y
  | Some err -> raise (Error err)

let wrap5 ctx f x1 x2 x3 x4 x5 =
  let y = f x1 x2 x3 x4 x5 in
  match gcc_jit_context_get_first_error ctx with
  | None -> y
  | Some err -> raise (Error err)

let wrap6 ctx f x1 x2 x3 x4 x5 x6 =
  let y = f x1 x2 x3 x4 x5 x6 in
  match gcc_jit_context_get_first_error ctx with
  | None -> y
  | Some err -> raise (Error err)

let wrap8 ctx f x1 x2 x3 x4 x5 x6 x7 x8 =
  let y = f x1 x2 x3 x4 x5 x6 x7 x8 in
  match gcc_jit_context_get_first_error ctx with
  | None -> y
  | Some err -> raise (Error err)

let get_first_error ctx =
  gcc_jit_context_get_first_error ctx

let acquire_context ctx =
  gcc_jit_context_acquire ctx

let release_context ctx =
  gcc_jit_context_release ctx

let new_child_context ctx =
  wrap1 ctx gcc_jit_context_new_child_context ctx

let dump_to_file ctx ?(update_locs = false) path =
  wrap3 ctx gcc_jit_context_dump_to_file ctx path (if update_locs then 1 else 0)

external int_of_file_descr : Unix.file_descr -> int = "%identity"

let set_logfile ctx = function
  | None ->
      wrap4 ctx gcc_jit_context_set_logfile ctx Ctypes.null 0 0
  | Some fd ->
      let f = match fdopen (int_of_file_descr fd) "a" with
        | None -> raise (Error "fdopen")
        | Some f -> f
      in
      wrap4 ctx gcc_jit_context_set_logfile ctx f 0 0

let dump_reproducer_to_file ctx path =
  wrap2 ctx gcc_jit_context_dump_reproducer_to_file ctx path

let get_debug_string obj =
  gcc_jit_object_get_debug_string (object' obj)

let set_option : type a. context -> a context_option -> a -> unit = fun ctx opt v ->
  match opt with
  | Progname ->
      wrap3 ctx gcc_jit_context_set_str_option ctx GCC_JIT_STR_OPTION_PROGNAME v
  | Optimization_level ->
      wrap3 ctx gcc_jit_context_set_int_option ctx GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL v
  | Debuginfo ->
      wrap3 ctx gcc_jit_context_set_bool_option ctx GCC_JIT_BOOL_OPTION_DEBUGINFO v
  | Dump_initial_tree ->
      wrap3 ctx gcc_jit_context_set_bool_option ctx GCC_JIT_BOOL_OPTION_DUMP_INITIAL_TREE v
  | Dump_initial_gimple ->
      wrap3 ctx gcc_jit_context_set_bool_option ctx GCC_JIT_BOOL_OPTION_DUMP_INITIAL_GIMPLE v
  | Dump_generated_code ->
      wrap3 ctx gcc_jit_context_set_bool_option ctx GCC_JIT_BOOL_OPTION_DUMP_GENERATED_CODE v
  | Dump_summary ->
      wrap3 ctx gcc_jit_context_set_bool_option ctx GCC_JIT_BOOL_OPTION_DUMP_SUMMARY v
  | Dump_everything ->
      wrap3 ctx gcc_jit_context_set_bool_option ctx GCC_JIT_BOOL_OPTION_DUMP_EVERYTHING v
  | Selfcheck_gc ->
      wrap3 ctx gcc_jit_context_set_bool_option ctx GCC_JIT_BOOL_OPTION_SELFCHECK_GC v
  | Keep_intermediates ->
      wrap3 ctx gcc_jit_context_set_bool_option ctx GCC_JIT_BOOL_OPTION_KEEP_INTERMEDIATES v

module T = struct
  let standard ctx kind =
    `Type (wrap2 ctx gcc_jit_context_get_type ctx (type_kind kind))

  let int_gen ctx ?(signed = false) n =
    `Type (wrap3 ctx gcc_jit_context_get_int_type ctx (if signed then 1 else 0) n)

  let int ctx =
    standard ctx Int

  let pointer typ =
    let ctx = gcc_jit_object_get_context (gcc_jit_type_as_object (typ' typ)) in
    `Type (wrap1 ctx gcc_jit_type_get_pointer (typ' typ))

  let const typ =
    let ctx = gcc_jit_object_get_context (gcc_jit_type_as_object (typ' typ)) in
    `Type (wrap1 ctx gcc_jit_type_get_const (typ' typ))

  let volatile typ =
    let ctx = gcc_jit_object_get_context (gcc_jit_type_as_object (typ' typ)) in
    `Type (wrap1 ctx gcc_jit_type_get_volatile (typ' typ))

  let array ?(loc = null_loc) ctx typ n =
    `Type (wrap4 ctx gcc_jit_context_new_array_type ctx (loc' loc) (typ' typ) n)

  let function_ptr ?(loc = null_loc) ctx ?(variadic = false) args ret =
    let a = Ctypes.CArray.of_list gcc_jit_type (List.map typ' args) in
    `Type
      (wrap5 ctx gcc_jit_context_new_function_ptr_type ctx
         (loc' loc) (typ' ret) (List.length args) (Ctypes.CArray.start a)
         (if variadic then 1 else 0))

  let field ?(loc = null_loc) ctx typ name =
    `Field (wrap4 ctx gcc_jit_context_new_field ctx (loc' loc) (typ' typ) name)

  let struct_ ?(loc = null_loc) ctx name fields =
    let a = Ctypes.CArray.of_list gcc_jit_field (List.map (function `Field f -> f) fields) in
    `Struct
      (wrap3 ctx gcc_jit_context_new_struct_type ctx (loc' loc) name
         (List.length fields) (Ctypes.CArray.start a))

  let opaque_struct ?(loc = null_loc) ctx name =
    `Struct (wrap2 ctx gcc_jit_context_new_opaque_struct ctx (loc' loc) name)

  let set_fields ?(loc = null_loc) (`Struct struc) fields =
    let ctx =
      gcc_jit_object_get_context (gcc_jit_type_as_object (gcc_jit_struct_as_type struc))
    in
    let a = Ctypes.CArray.of_list gcc_jit_field (List.map (function `Field f -> f) fields) in
    wrap3 ctx gcc_jit_struct_set_fields struc (loc' loc)
      (List.length fields) (Ctypes.CArray.start a)

  let union ?(loc = null_loc) ctx name fields =
    let a = Ctypes.CArray.of_list gcc_jit_field (List.map (function `Field f -> f) fields) in
    `Type
      (wrap3 ctx gcc_jit_context_new_union_type ctx (loc' loc) name
         (List.length fields) (Ctypes.CArray.start a))
end

module RV = struct
  let type_of rval =
    let ctx = gcc_jit_object_get_context (gcc_jit_rvalue_as_object (rvalue' rval)) in
    `Type (wrap1 ctx gcc_jit_rvalue_get_type (rvalue' rval)) (* CHECK *)

  let int ctx typ n =
    `Rvalue (wrap3 ctx gcc_jit_context_new_rvalue_from_int ctx (typ' typ) n)

  let zero (ctx : context) (typ : type_) : rvalue =
    `Rvalue (wrap2 ctx gcc_jit_context_zero ctx (typ' typ))

  let one ctx typ =
    `Rvalue (wrap2 ctx gcc_jit_context_one ctx (typ' typ))

  let double ctx typ f =
    `Rvalue (wrap3 ctx gcc_jit_context_new_rvalue_from_double ctx (typ' typ) f)

  let ptr ctx typ ptr =
    `Rvalue
      (wrap3 ctx gcc_jit_context_new_rvalue_from_ptr ctx (typ' typ)
         (Ctypes.to_voidp ptr))

  let null ctx typ =
    `Rvalue (wrap2 ctx gcc_jit_context_null ctx (typ' typ))

  let string_literal ctx str =
    `Rvalue (wrap2 ctx gcc_jit_context_new_string_literal ctx str)

  let unary_op ?(loc = null_loc) ctx op typ rval =
    `Rvalue
      (wrap5 ctx gcc_jit_context_new_unary_op ctx
         (loc' loc) (unary_op op) (typ' typ) (rvalue' rval))

  let binary_op ?(loc = null_loc) ctx op typ rval1 rval2 =
    `Rvalue
      (wrap6 ctx gcc_jit_context_new_binary_op ctx (loc' loc) (binary_op op)
         (typ' typ) (rvalue' rval1) (rvalue' rval2))

  let comparison ?(loc = null_loc) ctx cmp rval1 rval2 =
    `Rvalue
      (wrap5 ctx gcc_jit_context_new_comparison ctx (loc' loc) (comparison cmp)
         (rvalue' rval1) (rvalue' rval2))

  let call ?(loc = null_loc) ctx (`Function fn) args =
    let a = Ctypes.CArray.of_list gcc_jit_rvalue (List.map rvalue' args) in
    `Rvalue
      (wrap5 ctx gcc_jit_context_new_call ctx
         (loc' loc) fn (List.length args) (Ctypes.CArray.start a))

  let indirect_call ?(loc = null_loc) ctx rval args =
    let a = Ctypes.CArray.of_list gcc_jit_rvalue (List.map rvalue' args) in
    `Rvalue
      (wrap5 ctx gcc_jit_context_new_call_through_ptr ctx
         (loc' loc) (rvalue' rval) (List.length args) (Ctypes.CArray.start a))

  let cast ?(loc = null_loc) ctx rval typ =
    `Rvalue (wrap4 ctx gcc_jit_context_new_cast ctx (loc' loc) (rvalue' rval) (typ' typ))

  let param param =
    (param :> rvalue)
end

module LV = struct
  let address ?(loc = null_loc) lval =
    let ctx = gcc_jit_object_get_context (gcc_jit_lvalue_as_object (lvalue' lval)) in
    `Rvalue (wrap2 ctx gcc_jit_lvalue_get_address (lvalue' lval) (loc' loc))

  let global ?(loc = null_loc) ctx kind typ name =
    `Lvalue
      (wrap4 ctx gcc_jit_context_new_global ctx
         (loc' loc) (global_kind kind) (typ' typ) name)

  let deref ?(loc = null_loc) rval =
    let ctx = gcc_jit_object_get_context (gcc_jit_rvalue_as_object (rvalue' rval)) in
    `Lvalue (wrap2 ctx gcc_jit_rvalue_dereference (rvalue' rval) (loc' loc))

  let deref_field ?(loc = null_loc) rval (`Field fld) =
    let ctx = gcc_jit_object_get_context (gcc_jit_rvalue_as_object (rvalue' rval)) in
    `Lvalue (wrap3 ctx gcc_jit_rvalue_dereference_field (rvalue' rval) (loc' loc) fld)

  let array_access ?(loc = null_loc) rval1 rval2 =
    let ctx = gcc_jit_object_get_context (gcc_jit_rvalue_as_object (rvalue' rval1)) in
    `Lvalue
      (wrap3 ctx gcc_jit_context_new_array_access ctx (loc' loc)
         (rvalue' rval1) (rvalue' rval2))

  let param param =
    (param :> lvalue)
end

module Param = struct
  let create ?(loc = null_loc) ctx typ name =
    `Param (wrap4 ctx gcc_jit_context_new_param ctx (loc' loc) (typ' typ) name)
end

module Function = struct
  let create ?(loc = null_loc) ctx ?(variadic = false) kind ret name args =
    let a = Ctypes.CArray.of_list gcc_jit_param (List.map (function `Param p -> p) args) in
    `Function
      (wrap8 ctx gcc_jit_context_new_function
         ctx (loc' loc) (function_kind kind) (typ' ret) name (List.length args) (Ctypes.CArray.start a)
         (if variadic then 1 else 0))

  let builtin ctx name =
    `Function (wrap2 ctx gcc_jit_context_get_builtin_function ctx name)

  let param (`Function fn) i =
    let ctx = gcc_jit_object_get_context (gcc_jit_function_as_object fn) in
    `Param (wrap2 ctx gcc_jit_function_get_param fn i)

  let dump_dot (`Function fn) path =
    let ctx = gcc_jit_object_get_context (gcc_jit_function_as_object fn) in
    wrap2 ctx gcc_jit_function_dump_to_dot fn path

  let local ?(loc = null_loc) (`Function fn) typ name =
    let ctx = gcc_jit_object_get_context (gcc_jit_function_as_object fn) in
    `Lvalue (wrap4 ctx gcc_jit_function_new_local fn (loc' loc) (typ' typ) name)
end

let new_block (`Function fn) ?name () =
  let ctx = gcc_jit_object_get_context (gcc_jit_function_as_object fn) in
  `Block (wrap2 ctx gcc_jit_function_new_block fn name)

let get_function (`Block blk) =
  let ctx = gcc_jit_object_get_context (gcc_jit_block_as_object blk) in
  `Function (wrap1 ctx gcc_jit_block_get_function blk)

let add_eval ?(loc = null_loc) (`Block blk) rval =
  let ctx = gcc_jit_object_get_context (gcc_jit_block_as_object blk) in
  wrap3 ctx gcc_jit_block_add_eval blk (loc' loc) (rvalue' rval)

let add_assignment ?(loc = null_loc) (`Block blk) lval rval =
  let ctx = gcc_jit_object_get_context (gcc_jit_block_as_object blk) in
  wrap4 ctx gcc_jit_block_add_assignment blk (loc' loc) (lvalue' lval) (rvalue' rval)

let add_assignment_op ?(loc = null_loc) (`Block blk) lval op rval =
  let ctx = gcc_jit_object_get_context (gcc_jit_block_as_object blk) in
  wrap5 ctx gcc_jit_block_add_assignment_op blk (loc' loc)
    (lvalue' lval) (binary_op op) (rvalue' rval)

let add_comment ?(loc = null_loc) (`Block blk) str =
  let ctx = gcc_jit_object_get_context (gcc_jit_block_as_object blk) in
  wrap3 ctx gcc_jit_block_add_comment blk (loc' loc) str

let end_with_conditional ?(loc = null_loc) (`Block blk) rval (`Block blk1) (`Block blk2) =
  let ctx = gcc_jit_object_get_context (gcc_jit_block_as_object blk) in
  wrap5 ctx gcc_jit_block_end_with_conditional blk (loc' loc)
    (rvalue' rval) blk1 blk2

let end_with_jump ?(loc = null_loc) (`Block blk) (`Block blk1) =
  let ctx = gcc_jit_object_get_context (gcc_jit_block_as_object blk) in
  wrap3 ctx gcc_jit_block_end_with_jump blk (loc' loc) blk1

let end_with_return ?(loc = null_loc) (`Block blk) rval =
  let ctx = gcc_jit_object_get_context (gcc_jit_block_as_object blk) in
  wrap3 ctx gcc_jit_block_end_with_return blk (loc' loc) (rvalue' rval)

let end_with_void_return ?(loc = null_loc) (`Block blk) =
  let ctx = gcc_jit_object_get_context (gcc_jit_block_as_object blk) in
  wrap2 ctx gcc_jit_block_end_with_void_return blk (loc' loc)

let new_location ctx path line col =
  `Location (wrap4 ctx gcc_jit_context_new_location ctx path line col)

let compile ctx =
  let res = wrap1 ctx gcc_jit_context_compile ctx in
  Gc.finalise gcc_jit_result_release res;
  res

let get_code res name fn =
  let p = gcc_jit_result_get_code res name in
  Ctypes.(coerce (ptr void) (Foreign.funptr ~name fn) p)

let get_global res name typ =
  let p = gcc_jit_result_get_global res name in
  Ctypes.(coerce (ptr void) (ptr typ)) p

let release_result res =
  gcc_jit_result_release res

let output_kind = function
  | Assembler -> GCC_JIT_OUTPUT_KIND_ASSEMBLER
  | Object_file -> GCC_JIT_OUTPUT_KIND_OBJECT_FILE
  | Dynamic_library -> GCC_JIT_OUTPUT_KIND_DYNAMIC_LIBRARY
  | Executable -> GCC_JIT_OUTPUT_KIND_EXECUTABLE

let compile_to_file ctx kind path =
  wrap3 ctx gcc_jit_context_compile_to_file ctx (output_kind kind) path
