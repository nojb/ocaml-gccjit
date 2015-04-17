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
type location = gcc_jit_location
type param = gcc_jit_param
type lvalue = gcc_jit_lvalue
type rvalue = gcc_jit_rvalue
type field = gcc_jit_field
type struct_ = gcc_jit_struct
type type_ = gcc_jit_type
type function_ = gcc_jit_function
type block = gcc_jit_block

let null_loc = Ctypes.(coerce (ptr void) gcc_jit_location null)

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

type comparison = Eq | Ne | Lt | Le | Gt | Ge

type global_kind =
    GLOBAL_Exported
  | GLOBAL_Internal
  | GLOBAL_Imported

type output_kind =
    OUTPUT_Assembler
  | OUTPUT_Object_file
  | OUTPUT_Dynamic_library
  | OUTPUT_Executable

type type_kind =
    TYPE_Void
  | TYPE_Void_ptr
  | TYPE_Bool
  | TYPE_Char
  | TYPE_Signed_char
  | TYPE_Unsigned_char
  | TYPE_Short
  | TYPE_Unsigned_short
  | TYPE_Int
  | TYPE_Unsigned_int
  | TYPE_Long
  | TYPE_Unsigned_long
  | TYPE_Long_long
  | TYPE_Unsigned_long_long
  | TYPE_Float
  | TYPE_Double
  | TYPE_Long_double
  | TYPE_Const_char_ptr
  | TYPE_Size_t
  | TYPE_File_ptr
  | TYPE_Complex_float
  | TYPE_Complex_double
  | TYPE_Complex_long_double

let type_kind = function
    TYPE_Void -> GCC_JIT_TYPE_VOID
  | TYPE_Void_ptr -> GCC_JIT_TYPE_VOID_PTR
  | TYPE_Bool -> GCC_JIT_TYPE_BOOL
  | TYPE_Char -> GCC_JIT_TYPE_CHAR
  | TYPE_Signed_char -> GCC_JIT_TYPE_SIGNED_CHAR
  | TYPE_Unsigned_char -> GCC_JIT_TYPE_UNSIGNED_CHAR
  | TYPE_Short -> GCC_JIT_TYPE_SHORT
  | TYPE_Unsigned_short -> GCC_JIT_TYPE_UNSIGNED_SHORT
  | TYPE_Int -> GCC_JIT_TYPE_INT
  | TYPE_Unsigned_int -> GCC_JIT_TYPE_UNSIGNED_INT
  | TYPE_Long -> GCC_JIT_TYPE_LONG
  | TYPE_Unsigned_long -> GCC_JIT_TYPE_UNSIGNED_LONG
  | TYPE_Long_long -> GCC_JIT_TYPE_LONG_LONG
  | TYPE_Unsigned_long_long -> GCC_JIT_TYPE_UNSIGNED_LONG_LONG
  | TYPE_Float -> GCC_JIT_TYPE_FLOAT
  | TYPE_Double -> GCC_JIT_TYPE_DOUBLE
  | TYPE_Long_double -> GCC_JIT_TYPE_LONG_DOUBLE
  | TYPE_Const_char_ptr -> GCC_JIT_TYPE_CONST_CHAR_PTR
  | TYPE_Size_t -> GCC_JIT_TYPE_SIZE_T
  | TYPE_File_ptr -> GCC_JIT_TYPE_FILE_PTR
  | TYPE_Complex_float -> GCC_JIT_TYPE_COMPLEX_FLOAT
  | TYPE_Complex_double -> GCC_JIT_TYPE_COMPLEX_DOUBLE
  | TYPE_Complex_long_double -> GCC_JIT_TYPE_COMPLEX_LONG_DOUBLE

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
  | GLOBAL_Exported -> GCC_JIT_GLOBAL_EXPORTED
  | GLOBAL_Imported -> GCC_JIT_GLOBAL_IMPORTED
  | GLOBAL_Internal -> GCC_JIT_GLOBAL_INTERNAL

let output_kind = function
  | OUTPUT_Assembler -> GCC_JIT_OUTPUT_KIND_ASSEMBLER
  | OUTPUT_Object_file -> GCC_JIT_OUTPUT_KIND_OBJECT_FILE
  | OUTPUT_Dynamic_library -> GCC_JIT_OUTPUT_KIND_DYNAMIC_LIBRARY
  | OUTPUT_Executable -> GCC_JIT_OUTPUT_KIND_EXECUTABLE

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

module Context = struct
  let create () =
    gcc_jit_context_acquire ()

  let release ctx =
    gcc_jit_context_release ctx

  let create_child ctx =
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

  type _ context_option =
      Progname : string context_option
    | Optimization_level : int context_option
    | Debuginfo : bool context_option
    | Dump_initial_tree : bool context_option
    | Dump_initial_gimple : bool context_option
    | Dump_generated_code : bool context_option
    | Dump_summary : bool context_option
    | Dump_everything : bool context_option
    | Selfcheck_gc : bool context_option
    | Keep_intermediates : bool context_option

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

  let compile ctx =
    let res = wrap1 ctx gcc_jit_context_compile ctx in
    Gc.finalise gcc_jit_result_release res;
    res

  let compile_to_file ctx kind path =
    wrap3 ctx gcc_jit_context_compile_to_file ctx (output_kind kind) path
end

module Field = struct
  let create ctx ?(loc = null_loc) typ name =
    wrap4 ctx gcc_jit_context_new_field ctx loc typ name

  let to_string fld =
    gcc_jit_object_get_debug_string (gcc_jit_field_as_object fld)
end

module Struct = struct
  let create ctx ?(loc = null_loc) name fields =
    let a = Ctypes.CArray.of_list gcc_jit_field fields in
    wrap3 ctx gcc_jit_context_new_struct_type ctx loc name
      (List.length fields) (Ctypes.CArray.start a)

  let opaque ctx ?(loc = null_loc) name =
    wrap2 ctx gcc_jit_context_new_opaque_struct ctx loc name

  let set_fields ?(loc = null_loc) struc fields =
    let ctx =
      gcc_jit_object_get_context (gcc_jit_type_as_object (gcc_jit_struct_as_type struc))
    in
    let a = Ctypes.CArray.of_list gcc_jit_field fields in
    wrap3 ctx gcc_jit_struct_set_fields struc loc (List.length fields) (Ctypes.CArray.start a)

  let to_string struc =
    gcc_jit_object_get_debug_string (gcc_jit_type_as_object (gcc_jit_struct_as_type struc))
end

module Type = struct
  let standard ctx kind =
    wrap2 ctx gcc_jit_context_get_type ctx (type_kind kind)

  let int_gen ctx ?(signed = false) n =
    wrap3 ctx gcc_jit_context_get_int_type ctx (if signed then 1 else 0) n

  let int ctx =
    standard ctx TYPE_Int

  let pointer typ =
    let ctx = gcc_jit_object_get_context (gcc_jit_type_as_object typ) in
    wrap1 ctx gcc_jit_type_get_pointer typ

  let const typ =
    let ctx = gcc_jit_object_get_context (gcc_jit_type_as_object typ) in
    wrap1 ctx gcc_jit_type_get_const typ

  let volatile typ =
    let ctx = gcc_jit_object_get_context (gcc_jit_type_as_object typ) in
    wrap1 ctx gcc_jit_type_get_volatile typ

  let array ctx ?(loc = null_loc) typ n =
    wrap4 ctx gcc_jit_context_new_array_type ctx loc typ n

  let function_ptr ctx ?(loc = null_loc) ?(variadic = false) args ret =
    let a = Ctypes.CArray.of_list gcc_jit_type args in
    wrap5 ctx gcc_jit_context_new_function_ptr_type ctx
      loc ret (List.length args) (Ctypes.CArray.start a)
      (if variadic then 1 else 0)

  let struct_ str =
    let ctx = gcc_jit_object_get_context (gcc_jit_type_as_object (gcc_jit_struct_as_type str)) in
    wrap1 ctx gcc_jit_struct_as_type str

  let union ctx ?(loc = null_loc) name fields =
    let a = Ctypes.CArray.of_list gcc_jit_field fields in
    wrap3 ctx gcc_jit_context_new_union_type ctx loc name (List.length fields) (Ctypes.CArray.start a)

  let to_string typ =
    gcc_jit_object_get_debug_string (gcc_jit_type_as_object typ)
end

module RValue = struct
  let type_of rval =
    let ctx = gcc_jit_object_get_context (gcc_jit_rvalue_as_object rval) in
    wrap1 ctx gcc_jit_rvalue_get_type rval

  let int ctx typ n =
    wrap3 ctx gcc_jit_context_new_rvalue_from_int ctx typ n

  let zero ctx typ : rvalue =
    wrap2 ctx gcc_jit_context_zero ctx typ

  let one ctx typ =
    wrap2 ctx gcc_jit_context_one ctx typ

  let double ctx typ f =
    wrap3 ctx gcc_jit_context_new_rvalue_from_double ctx typ f

  let ptr ctx typ ptr =
    wrap3 ctx gcc_jit_context_new_rvalue_from_ptr ctx typ
      (Ctypes.to_voidp ptr)

  let null ctx typ =
    wrap2 ctx gcc_jit_context_null ctx typ

  let string_literal ctx str =
    wrap2 ctx gcc_jit_context_new_string_literal ctx str

  let unary_op ctx ?(loc = null_loc) op typ rval =
    wrap5 ctx gcc_jit_context_new_unary_op ctx
      loc (unary_op op) typ rval

  let binary_op ctx ?(loc = null_loc) op typ rval1 rval2 =
    wrap6 ctx gcc_jit_context_new_binary_op ctx loc (binary_op op)
      typ rval1 rval2

  let comparison ctx ?(loc = null_loc) cmp rval1 rval2 =
    wrap5 ctx gcc_jit_context_new_comparison ctx loc (comparison cmp) rval1 rval2

  let call ctx ?(loc = null_loc) fn args =
    let a = Ctypes.CArray.of_list gcc_jit_rvalue args in
    wrap5 ctx gcc_jit_context_new_call ctx loc fn (List.length args) (Ctypes.CArray.start a)

  let indirect_call ctx ?(loc = null_loc) rval args =
    let a = Ctypes.CArray.of_list gcc_jit_rvalue args in
    wrap5 ctx gcc_jit_context_new_call_through_ptr ctx
      loc rval (List.length args) (Ctypes.CArray.start a)

  let cast ctx ?(loc = null_loc) rval typ =
    wrap4 ctx gcc_jit_context_new_cast ctx loc rval typ

  let access_field ?(loc = null_loc) rval fld =
    let ctx = gcc_jit_object_get_context (gcc_jit_rvalue_as_object rval) in
    wrap3 ctx gcc_jit_rvalue_access_field rval loc fld

  let lvalue lval =
    let ctx = gcc_jit_object_get_context (gcc_jit_lvalue_as_object lval) in
    wrap1 ctx gcc_jit_lvalue_as_rvalue lval

  let param param =
    let ctx = gcc_jit_object_get_context (gcc_jit_param_as_object param) in
    wrap1 ctx gcc_jit_param_as_rvalue param

  let to_string rval =
    gcc_jit_object_get_debug_string (gcc_jit_rvalue_as_object rval)
end

module LValue = struct
  let address ?(loc = null_loc) lval =
    let ctx = gcc_jit_object_get_context (gcc_jit_lvalue_as_object lval) in
    wrap2 ctx gcc_jit_lvalue_get_address lval loc

  let global ctx ?(loc = null_loc) kind typ name =
    wrap4 ctx gcc_jit_context_new_global ctx loc (global_kind kind) typ name

  let deref ?(loc = null_loc) rval =
    let ctx = gcc_jit_object_get_context (gcc_jit_rvalue_as_object rval) in
    wrap2 ctx gcc_jit_rvalue_dereference rval loc

  let deref_field ?(loc = null_loc) rval fld =
    let ctx = gcc_jit_object_get_context (gcc_jit_rvalue_as_object rval) in
    wrap3 ctx gcc_jit_rvalue_dereference_field rval loc fld

  let access_array ?(loc = null_loc) rval1 rval2 =
    let ctx = gcc_jit_object_get_context (gcc_jit_rvalue_as_object rval1) in
    wrap3 ctx gcc_jit_context_new_array_access ctx loc rval1 rval2

  let access_field ?(loc = null_loc) lval fld =
    let ctx = gcc_jit_object_get_context (gcc_jit_lvalue_as_object lval) in
    wrap3 ctx gcc_jit_lvalue_access_field lval loc fld

  let param param =
    let ctx = gcc_jit_object_get_context (gcc_jit_param_as_object param) in
    wrap1 ctx gcc_jit_param_as_lvalue param

  let to_string lval =
    gcc_jit_object_get_debug_string (gcc_jit_lvalue_as_object lval)
end

module Param = struct
  let create ctx ?(loc = null_loc) typ name =
    wrap4 ctx gcc_jit_context_new_param ctx loc typ name

  let to_string param =
    gcc_jit_object_get_debug_string (gcc_jit_param_as_object param)
end

module Function = struct

  type function_kind =
      Exported
    | Internal
    | Imported
    | Always_inline

  let function_kind = function
      Exported -> GCC_JIT_FUNCTION_EXPORTED
    | Internal -> GCC_JIT_FUNCTION_INTERNAL
    | Imported -> GCC_JIT_FUNCTION_IMPORTED
    | Always_inline -> GCC_JIT_FUNCTION_ALWAYS_INLINE

  let create ctx ?(loc = null_loc) ?(variadic = false) kind ret name args =
    let a = Ctypes.CArray.of_list gcc_jit_param args in
    wrap8 ctx gcc_jit_context_new_function
      ctx loc (function_kind kind) ret name (List.length args) (Ctypes.CArray.start a)
      (if variadic then 1 else 0)

  let builtin ctx name =
    wrap2 ctx gcc_jit_context_get_builtin_function ctx name

  let param fn i =
    let ctx = gcc_jit_object_get_context (gcc_jit_function_as_object fn) in
    wrap2 ctx gcc_jit_function_get_param fn i

  let dump_dot fn path =
    let ctx = gcc_jit_object_get_context (gcc_jit_function_as_object fn) in
    wrap2 ctx gcc_jit_function_dump_to_dot fn path

  let local ?(loc = null_loc) fn typ name =
    let ctx = gcc_jit_object_get_context (gcc_jit_function_as_object fn) in
    wrap4 ctx gcc_jit_function_new_local fn loc typ name

  let to_string fn =
    gcc_jit_object_get_debug_string (gcc_jit_function_as_object fn)
end

module Block = struct
  let create ?name fn =
    let ctx = gcc_jit_object_get_context (gcc_jit_function_as_object fn) in
    wrap2 ctx gcc_jit_function_new_block fn name

  let parent blk =
    let ctx = gcc_jit_object_get_context (gcc_jit_block_as_object blk) in
    wrap1 ctx gcc_jit_block_get_function blk

  let eval ?(loc = null_loc) blk rval =
    let ctx = gcc_jit_object_get_context (gcc_jit_block_as_object blk) in
    wrap3 ctx gcc_jit_block_add_eval blk loc rval

  let assign ?(loc = null_loc) blk lval rval =
    let ctx = gcc_jit_object_get_context (gcc_jit_block_as_object blk) in
    wrap4 ctx gcc_jit_block_add_assignment blk loc lval rval

  let assign_op ?(loc = null_loc) blk lval op rval =
    let ctx = gcc_jit_object_get_context (gcc_jit_block_as_object blk) in
    wrap5 ctx gcc_jit_block_add_assignment_op blk loc lval (binary_op op) rval

  let comment ?(loc = null_loc) blk str =
    let ctx = gcc_jit_object_get_context (gcc_jit_block_as_object blk) in
    wrap3 ctx gcc_jit_block_add_comment blk loc str

  let cond_jump ?(loc = null_loc) blk rval blk1 blk2 =
    let ctx = gcc_jit_object_get_context (gcc_jit_block_as_object blk) in
    wrap5 ctx gcc_jit_block_end_with_conditional blk loc rval blk1 blk2

  let jump ?(loc = null_loc) blk blk1 =
    let ctx = gcc_jit_object_get_context (gcc_jit_block_as_object blk) in
    wrap3 ctx gcc_jit_block_end_with_jump blk loc blk1

  let return ?(loc = null_loc) blk rval =
    let ctx = gcc_jit_object_get_context (gcc_jit_block_as_object blk) in
    wrap3 ctx gcc_jit_block_end_with_return blk loc rval

  let return_void ?(loc = null_loc) blk =
    let ctx = gcc_jit_object_get_context (gcc_jit_block_as_object blk) in
    wrap2 ctx gcc_jit_block_end_with_void_return blk loc

  let to_string blk =
    gcc_jit_object_get_debug_string (gcc_jit_block_as_object blk)
end

module Location = struct
  let create ctx path line col =
    wrap4 ctx gcc_jit_context_new_location ctx path line col

  let to_string loc =
    gcc_jit_object_get_debug_string (gcc_jit_location_as_object loc)
end

module Result = struct
  let code res name fn =
    let p = gcc_jit_result_get_code res name in
    Ctypes.(coerce (ptr void) (Foreign.funptr ~name fn) p)

  let global res name typ =
    let p = gcc_jit_result_get_global res name in
    Ctypes.(coerce (ptr void) (ptr typ)) p

  let release res =
    gcc_jit_result_release res
end


module type S = sig
  module Context : sig
    val release : unit -> unit
    val dump_to_file : ?update_locs:bool -> string -> unit
    val set_logfile : Unix.file_descr option -> unit
    val dump_reproducer_to_file : string -> unit
    type _ context_option =
        Progname : string context_option
      | Optimization_level : int context_option
      | Debuginfo : bool context_option
      | Dump_initial_tree : bool context_option
      | Dump_initial_gimple : bool context_option
      | Dump_generated_code : bool context_option
      | Dump_summary : bool context_option
      | Dump_everything : bool context_option
      | Selfcheck_gc : bool context_option
      | Keep_intermediates : bool context_option
    val set_option : 'a context_option -> 'a -> unit
    val compile : unit -> result
    val compile_to_file : output_kind -> string -> unit
  end

  module Field : sig
    val create : ?loc:location -> type_ -> string -> field
    val to_string : field -> string
  end

  module Struct : sig
    val create : ?loc:location -> string -> field list -> struct_
    val opaque : ?loc:location -> string -> struct_
    val set_fields : ?loc:location -> struct_ -> field list -> unit
    val to_string : struct_ -> string
  end

  module Type : sig
    val standard : type_kind -> type_
    val int_gen : ?signed:bool -> int -> type_
    val int : type_
    val pointer : type_ -> type_
    val const : type_ -> type_
    val volatile : type_ -> type_
    val array : ?loc:location -> type_ -> int -> type_
    val function_ptr : ?loc:location -> ?variadic:bool -> type_ list -> type_ -> type_
    val struct_ : struct_ -> type_
    val union : ?loc:location -> string -> field list -> type_
    val to_string : type_ -> string
  end

  module RValue : sig
    val type_of : rvalue -> type_
    val int : type_ -> int -> rvalue
    val zero : type_ -> rvalue
    val one : type_ -> rvalue
    val double : type_ -> float -> rvalue
    val ptr : type_ -> 'a Ctypes.ptr -> rvalue
    val null : type_ -> rvalue
    val string_literal : string -> rvalue
    val unary_op : ?loc:location -> unary_op -> type_ -> rvalue -> rvalue
    val binary_op : ?loc:location -> binary_op -> type_ -> rvalue -> rvalue -> rvalue
    val comparison : ?loc:location -> comparison -> rvalue -> rvalue -> rvalue
    val call : ?loc:location -> function_ -> rvalue list -> rvalue
    val indirect_call : ?loc:location -> rvalue -> rvalue list -> rvalue
    val cast : ?loc:location -> rvalue -> type_ -> rvalue
    val access_field : ?loc:location -> rvalue -> field -> rvalue
    val lvalue : lvalue -> rvalue
    val param : param -> rvalue
    val to_string : rvalue -> string
  end

  module LValue : sig
    val address : ?loc:location -> lvalue -> rvalue
    val global : ?loc:location -> global_kind -> type_ -> string -> lvalue
    val deref : ?loc:location -> rvalue -> lvalue
    val deref_field : ?loc:location -> rvalue -> field -> lvalue
    val access_array : ?loc:location -> rvalue -> rvalue -> lvalue
    val access_field : ?loc:location -> lvalue -> field -> lvalue
    val param : param -> lvalue
    val to_string : lvalue -> string
  end

  module Param : sig
    val create : ?loc:location -> type_ -> string -> param
    val to_string : param -> string
  end

  module Function : sig
    type function_kind =
        Exported
      | Internal
      | Imported
      | Always_inline
    val create : ?loc:location -> ?variadic:bool -> function_kind -> type_ -> string -> param list -> function_
    val builtin : string -> function_
    val param : function_ -> int -> param
    val dump_dot : function_ -> string -> unit
    val local : ?loc:location -> function_ -> type_ -> string -> lvalue
    val to_string : function_ -> string
  end

  module Block : sig
    val create : ?name:string -> function_ -> block
    val parent : block -> function_
    val eval : ?loc:location -> block -> rvalue -> unit
    val assign : ?loc:location -> block -> lvalue -> rvalue -> unit
    val assign_op : ?loc:location -> block -> lvalue -> binary_op -> rvalue -> unit
    val comment : ?loc:location -> block -> string -> unit
    val cond_jump : ?loc:location -> block -> rvalue -> block -> block -> unit
    val jump : ?loc:location -> block -> block -> unit
    val return : ?loc:location -> block -> rvalue -> unit
    val return_void : ?loc:location -> block -> unit
    val to_string : block -> string
  end

  module Location : sig
    val create : string -> int -> int -> location
    val to_string : location -> string
  end

  module Result : sig
    val code : result -> string -> ('a -> 'b) Ctypes.fn -> 'a -> 'b
    val global : result -> string -> 'a Ctypes.typ -> 'a Ctypes.ptr
    val release : result -> unit
  end
end

module Make () = struct
  let ctx = Context.create ()

  module Context = struct
    open Context
    let release () = release ctx
    let dump_to_file = dump_to_file ctx
    let set_logfile = set_logfile ctx
    let dump_reproducer_to_file = dump_reproducer_to_file ctx
    type 'a context_option = 'a Context.context_option =
        Progname : string context_option
      | Optimization_level : int context_option
      | Debuginfo : bool context_option
      | Dump_initial_tree : bool context_option
      | Dump_initial_gimple : bool context_option
      | Dump_generated_code : bool context_option
      | Dump_summary : bool context_option
      | Dump_everything : bool context_option
      | Selfcheck_gc : bool context_option
      | Keep_intermediates : bool context_option
    let set_option o v = set_option ctx o v
    let compile () = compile ctx
    let compile_to_file = compile_to_file ctx
  end

  module Field = struct
    open Field
    let create = create ctx
    let to_string = to_string
  end

  module Struct = struct
    open Struct
    let create = create ctx
    let opaque = opaque ctx
    let set_fields = set_fields
    let to_string = to_string
  end

  module Type = struct
    open Type
    let standard = standard ctx
    let int_gen = int_gen ctx
    let int = int ctx
    let pointer = pointer
    let const = const
    let volatile = volatile
    let array = array ctx
    let function_ptr = function_ptr ctx
    let struct_ = struct_
    let union = union ctx
    let to_string = to_string
  end

  module RValue = struct
    open RValue
    let type_of = type_of
    let int = int ctx
    let zero = zero ctx
    let one = one ctx
    let double = double ctx
    let ptr typ p = ptr ctx typ p
    let null = null ctx
    let string_literal = string_literal ctx
    let unary_op = unary_op ctx
    let binary_op = binary_op ctx
    let comparison = comparison ctx
    let call = call ctx
    let indirect_call = indirect_call ctx
    let cast = cast ctx
    let access_field = access_field
    let lvalue = lvalue
    let param = param
    let to_string = to_string
  end

  module LValue = struct
    open LValue
    let address = address
    let global = global ctx
    let deref = deref
    let deref_field = deref_field
    let access_array = access_array
    let access_field = access_field
    let param = param
    let to_string = to_string
  end

  module Param = struct
    open Param
    let create = create ctx
    let to_string = to_string
  end

  module Function = struct
    type function_kind = Function.function_kind =
        Exported
      | Internal
      | Imported
      | Always_inline
    open Function
    let create = create ctx
    let builtin = builtin ctx
    let param = param
    let dump_dot = dump_dot
    let local = local
    let to_string = to_string
  end

  module Block = struct
    open Block
    let create = create
    let parent = parent
    let eval = eval
    let assign = assign
    let assign_op = assign_op
    let comment = comment
    let cond_jump = cond_jump
    let jump = jump
    let return = return
    let return_void = return_void
    let to_string = to_string
  end

  module Location = struct
    open Location
    let create = create ctx
    let to_string = to_string
  end

  module Result = struct
    open Result
    let code = code
    let global = global
    let release = release
  end
end
