exception Error of string * string

module B = Gccjit_stubs.Bindings (Gccjit_enums_gen) (Gccjit_generated)

open Gccjit_stubs
open B

type context = gcc_jit_context
type result = gcc_jit_result
type loc = gcc_jit_location
type lvalue = gcc_jit_lvalue
type rvalue = gcc_jit_rvalue
type field = gcc_jit_field
type typ = gcc_jit_type
type structure = gcc_jit_struct
type param = gcc_jit_param
type fn = gcc_jit_function
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

type _ option =
  | Progname : string option
  | Optimization_level : int option
  | Debuginfo : bool option
  | Dump_initial_tree : bool option
  | Dump_initial_gimple : bool option
  | Dump_generated_code : bool option
  | Dump_summary : bool option
  | Dump_everything : bool option
  | Selfcheck_gc : bool option
  | Keep_intermediaries : bool option

type output_kind =
  | Assembler
  | Object_file
  | Dynamic_library
  | Executable

let wrap1 name ctx f x1 =
  let y = f x1 in
  match gcc_jit_context_get_first_error ctx with
  | None -> y
  | Some err -> raise (Error (name, err))

let wrap2 name ctx f x1 x2 =
  let y = f x1 x2 in
  match gcc_jit_context_get_first_error ctx with
  | None -> y
  | Some err -> raise (Error (name, err))

let wrap3 name ctx f x1 x2 x3 =
  let y = f x1 x2 x3 in
  match gcc_jit_context_get_first_error ctx with
  | None -> y
  | Some err -> raise (Error (name, err))

let wrap4 name ctx f x1 x2 x3 x4 =
  let y = f x1 x2 x3 x4 in
  match gcc_jit_context_get_first_error ctx with
  | None -> y
  | Some err -> raise (Error (name, err))

let acquire =
  gcc_jit_context_acquire

let release =
  gcc_jit_context_release

let dump_to_file ctx ?(update_locs = false) path =
  wrap3 "dump_to_file" ctx gcc_jit_context_dump_to_file ctx path (if update_locs then 1 else 0)

(* val get_first_error : context -> string option *)

let new_location ctx path line col =
  wrap4 "new_location" ctx gcc_jit_context_new_location ctx path line col

let new_global ?(loc = null_loc) ctx typ name =
  wrap4 "new_global" ctx gcc_jit_context_new_global ctx loc GCC_JIT_GLOBAL_EXPORTED typ name

(* val new_array_type : ?loc:loc -> context -> typ -> int -> typ *)
(* val new_field : ?loc:loc -> context -> typ -> field *)
(* val new_struct : ?loc:loc -> context -> field list -> structure *)
(* val new_union : ?loc:loc -> context -> field list -> typ *)
(* val new_function_ptr_type : ?loc:loc -> context -> ?variadic:bool -> typ list -> typ -> typ *)
(* val new_param : ?loc:loc -> context -> string -> typ -> param *)
(* val new_function : ?loc:loc -> context -> ?variadic:bool -> function_kind -> string -> param list -> typ -> fn *)

let get_builtin_function ctx name =
  wrap2 "new_builtin_function" ctx gcc_jit_context_get_builtin_function ctx name

let zero ctx typ =
  wrap2 "zero" ctx gcc_jit_context_zero ctx typ

let one ctx typ =
  wrap2 "one" ctx gcc_jit_context_one ctx typ

(* val new_rvalue_from_double : context -> typ -> float -> rvalue *)
(* val new_rvalue_from_int : context -> typ -> int -> rvalue *)
(* val new_rvalue_from_ptr : context -> typ -> int -> rvalue *)

let null ctx typ =
  wrap2 "null" ctx gcc_jit_context_null ctx typ

let new_string_literal ctx str =
  wrap2 "new_string_literal" ctx gcc_jit_context_new_string_literal ctx str

(* val new_unary_op : ?loc:loc -> context -> unary_op -> typ -> rvalue -> rvalue *)
(* val new_binary_op : ?loc:loc -> context -> binary_op -> typ -> rvalue -> rvalue -> rvalue *)
(* val new_comparison : ?loc:loc -> context -> comparison -> rvalue -> rvalue -> rvalue *)
(* val new_child_context : context -> context *)
(* val new_cast : ?loc:loc -> context -> rvalue -> typ -> rvalue *)
(* val new_array_access : ?loc:loc -> rvalue -> rvalue -> lvalue *)
(* val new_call : ?loc:loc -> context -> fn -> rvalue list -> rvalue *)
(* val new_call_through_ptr : ?loc:loc -> context -> rvalue -> rvalue list -> rvalue *)
(* val get_int_type : context -> ?signed:bool -> int -> typ *)
(* val dump_reproducer_to_file : context -> string -> unit *)
(* val set_logfile : context -> out_channel -> unit *)
(* val set_option : context -> 'a option -> 'a -> unit *)

let output_kind = function
  | Assembler -> GCC_JIT_OUTPUT_KIND_ASSEMBLER
  | Object_file -> GCC_JIT_OUTPUT_KIND_OBJECT_FILE
  | Dynamic_library -> GCC_JIT_OUTPUT_KIND_DYNAMIC_LIBRARY
  | Executable -> GCC_JIT_OUTPUT_KIND_EXECUTABLE

let compile_to_file ctx kind path =
  wrap3 "compile_to_file" ctx gcc_jit_context_compile_to_file ctx (output_kind kind) path

let compile ctx =
  wrap1 "compile" ctx gcc_jit_context_compile ctx

(* val set_fields : ?loc:loc -> structure -> field list -> unit *)

let get_code res name fn =
  let p = gcc_jit_result_get_code res name in
  Ctypes.(coerce (ptr void) (Foreign.funptr ~name fn) p)

let pointer typ =
  let ctx = gcc_jit_object_get_context (gcc_jit_type_as_object typ) in
  wrap1 "pointer" ctx gcc_jit_type_get_pointer typ

let const typ =
  let ctx = gcc_jit_object_get_context (gcc_jit_type_as_object typ) in
  wrap1 "const" ctx gcc_jit_type_get_const typ

let volatile typ =
  let ctx = gcc_jit_object_get_context (gcc_jit_type_as_object typ) in
  wrap1 "volatile" ctx gcc_jit_type_get_volatile typ

let dereference_field ?(loc = null_loc) rval fld =
  let ctx = gcc_jit_object_get_context (gcc_jit_rvalue_as_object rval) in
  wrap3 "dereference_field" ctx gcc_jit_rvalue_dereference_field rval loc fld

let dereference ?(loc = null_loc) rval =
  let ctx = gcc_jit_object_get_context (gcc_jit_rvalue_as_object rval) in
  wrap2 "dereference" ctx gcc_jit_rvalue_dereference rval loc

let get_type rval =
  let ctx = gcc_jit_object_get_context (gcc_jit_rvalue_as_object rval) in
  wrap1 "get_type" ctx gcc_jit_rvalue_get_type rval

let get_address ?(loc = null_loc) lval =
  let ctx = gcc_jit_object_get_context (gcc_jit_lvalue_as_object lval) in
  wrap2 "get_address" ctx gcc_jit_lvalue_get_address lval loc

let new_local ?(loc = null_loc) fn typ name =
  let ctx = gcc_jit_object_get_context (gcc_jit_function_as_object fn) in
  wrap4 "new_local" ctx gcc_jit_function_new_local fn loc typ name

let new_block fn name =
  let ctx = gcc_jit_object_get_context (gcc_jit_function_as_object fn) in
  wrap2 "new_block" ctx gcc_jit_function_new_block fn name

let get_param fn i =
  let ctx = gcc_jit_object_get_context (gcc_jit_function_as_object fn) in
  wrap2 "get_param" ctx gcc_jit_function_get_param fn i

let dump_to_dot fn path =
  let ctx = gcc_jit_object_get_context (gcc_jit_function_as_object fn) in
  wrap2 "dump_to_dot" ctx gcc_jit_function_dump_to_dot fn path

(* val add_eval : ?loc:loc -> block -> rvalue -> unit *)
(* val add_assignment : ?loc:loc -> block -> lvalue -> rvalue -> unit *)
(* val add_assignment_op : ?loc:loc -> block -> lvalue -> binary_op -> rvalue -> unit *)
(* val add_comment : ?loc:loc -> block -> string -> unit *)
(* val get_function : block -> fn *)
