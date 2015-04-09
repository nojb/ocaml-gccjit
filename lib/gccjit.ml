exception Error of string * string

module B = Gccjit_stubs.Bindings (Gccjit_enums_gen) (Gccjit_generated)

open Gccjit_stubs
open B

type context = gcc_jit_context Ctypes.structure Ctypes.ptr
type result = gcc_jit_result Ctypes.structure Ctypes.ptr
type loc = gcc_jit_location Ctypes.structure Ctypes.ptr
type lvalue = gcc_jit_lvalue Ctypes.structure Ctypes.ptr
type rvalue = gcc_jit_rvalue Ctypes.structure Ctypes.ptr
type field = gcc_jit_field Ctypes.structure Ctypes.ptr
type typ = gcc_jit_type Ctypes.structure Ctypes.ptr
type structure = gcc_jit_struct Ctypes.structure Ctypes.ptr
type param = gcc_jit_param Ctypes.structure Ctypes.ptr
type fn = gcc_jit_function Ctypes.structure Ctypes.ptr
type block = gcc_jit_block Ctypes.structure Ctypes.ptr

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
  | Keep_intermediates : bool option

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

let wrap5 name ctx f x1 x2 x3 x4 x5 =
  let y = f x1 x2 x3 x4 x5 in
  match gcc_jit_context_get_first_error ctx with
  | None -> y
  | Some err -> raise (Error (name, err))

let wrap6 name ctx f x1 x2 x3 x4 x5 x6 =
  let y = f x1 x2 x3 x4 x5 x6 in
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

let new_array_type ?(loc = null_loc) ctx typ n =
  wrap4 "new_array_type" ctx gcc_jit_context_new_array_type ctx loc typ n

let new_field ?(loc = null_loc) ctx typ name =
  wrap4 "new_field" ctx gcc_jit_context_new_field ctx loc typ name

let new_struct ?(loc = null_loc) ctx name fields =
  let a = Ctypes.CArray.of_list gcc_jit_field fields in
  wrap3 "new_struct" ctx gcc_jit_context_new_struct_type ctx loc name
    (List.length fields) (Ctypes.CArray.start a)

let new_union ?(loc = null_loc) ctx name fields =
  let a = Ctypes.CArray.of_list gcc_jit_field fields in
  wrap3 "new_struct" ctx gcc_jit_context_new_union_type ctx loc name
    (List.length fields) (Ctypes.CArray.start a)

let new_function_ptr_type ?(loc = null_loc) ctx ?(variadic = false) args ret =
  let a = Ctypes.CArray.of_list gcc_jit_type args in
  wrap5 "new_function_ptr_type" ctx
    gcc_jit_context_new_function_ptr_type ctx loc ret (List.length args) (Ctypes.CArray.start a)
    (if variadic then 1 else 0)

let new_param ?(loc = null_loc) ctx name typ =
  wrap4 "new_param" ctx gcc_jit_context_new_param ctx loc typ name

let function_kind = function
  | Exported -> GCC_JIT_FUNCTION_EXPORTED
  | Internal -> GCC_JIT_FUNCTION_INTERNAL
  | Imported -> GCC_JIT_FUNCTION_IMPORTED
  | Always_inline -> GCC_JIT_FUNCTION_ALWAYS_INLINE

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

let unary_op = function
  | Negate -> GCC_JIT_UNARY_OP_MINUS
  | Bitwise_negate -> GCC_JIT_UNARY_OP_BITWISE_NEGATE
  | Logical_negate -> GCC_JIT_UNARY_OP_LOGICAL_NEGATE

let new_unary_op ?(loc = null_loc) ctx op typ rval =
  wrap5 "new_unary_op" ctx gcc_jit_context_new_unary_op ctx loc (unary_op op) typ rval

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

let new_binary_op ?(loc = null_loc) ctx op typ rval1 rval2 =
  wrap6 "new_binary_op" ctx gcc_jit_context_new_binary_op ctx loc (binary_op op) typ rval1 rval2

(* val new_comparison : ?loc:loc -> context -> comparison -> rvalue -> rvalue -> rvalue *)

let new_child_context ctx =
  wrap1 "new_child_context" ctx gcc_jit_context_new_child_context ctx

let new_cast ?(loc = null_loc) ctx rval typ =
  wrap4 "new_cast" ctx gcc_jit_context_new_cast ctx loc rval typ

let new_array_access ?(loc = null_loc) rval1 rval2 =
  let ctx = gcc_jit_object_get_context (gcc_jit_rvalue_as_object rval1) in
  wrap3 "new_array_access" ctx gcc_jit_context_new_array_access ctx loc rval1 rval2

let new_call ?(loc = null_loc) ctx fn args =
  let a = Ctypes.CArray.of_list gcc_jit_rvalue args in
  wrap5 "new_call" ctx gcc_jit_context_new_call ctx loc fn (List.length args) (Ctypes.CArray.start a)

let new_call_through_ptr ?(loc = null_loc) ctx rval args =
  let a = Ctypes.CArray.of_list gcc_jit_rvalue args in
  wrap5 "new_call_through_ptr" ctx gcc_jit_context_new_call_through_ptr ctx loc rval
    (List.length args) (Ctypes.CArray.start a)

(* val get_int_type : context -> ?signed:bool -> int -> typ *)
(* val dump_reproducer_to_file : context -> string -> unit *)

let set_logfile ctx fd =
  assert false

let set_option : type a. context -> a option -> a -> unit = fun ctx opt v ->
  match opt with
  | Progname ->
      wrap3 "set_option" ctx gcc_jit_context_set_str_option ctx GCC_JIT_STR_OPTION_PROGNAME v
  | Optimization_level ->
      wrap3 "set_option" ctx gcc_jit_context_set_int_option ctx GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL v
  | Debuginfo ->
      wrap3 "set_option" ctx gcc_jit_context_set_bool_option ctx GCC_JIT_BOOL_OPTION_DEBUGINFO v
  | Dump_initial_tree ->
      wrap3 "set_option" ctx gcc_jit_context_set_bool_option ctx GCC_JIT_BOOL_OPTION_DUMP_INITIAL_TREE v
  | Dump_initial_gimple ->
      wrap3 "set_option" ctx gcc_jit_context_set_bool_option ctx GCC_JIT_BOOL_OPTION_DUMP_INITIAL_GIMPLE v
  | Dump_generated_code ->
      wrap3 "set_option" ctx gcc_jit_context_set_bool_option ctx GCC_JIT_BOOL_OPTION_DUMP_GENERATED_CODE v
  | Dump_summary ->
      wrap3 "set_option" ctx gcc_jit_context_set_bool_option ctx GCC_JIT_BOOL_OPTION_DUMP_SUMMARY v
  | Dump_everything ->
      wrap3 "set_option" ctx gcc_jit_context_set_bool_option ctx GCC_JIT_BOOL_OPTION_DUMP_EVERYTHING v
  | Selfcheck_gc ->
      wrap3 "set_option" ctx gcc_jit_context_set_bool_option ctx GCC_JIT_BOOL_OPTION_SELFCHECK_GC v
  | Keep_intermediates ->
      wrap3 "set_option" ctx gcc_jit_context_set_bool_option ctx GCC_JIT_BOOL_OPTION_KEEP_INTERMEDIATES v

let output_kind = function
  | Assembler -> GCC_JIT_OUTPUT_KIND_ASSEMBLER
  | Object_file -> GCC_JIT_OUTPUT_KIND_OBJECT_FILE
  | Dynamic_library -> GCC_JIT_OUTPUT_KIND_DYNAMIC_LIBRARY
  | Executable -> GCC_JIT_OUTPUT_KIND_EXECUTABLE

let compile_to_file ctx kind path =
  wrap3 "compile_to_file" ctx gcc_jit_context_compile_to_file ctx (output_kind kind) path

let compile ctx =
  wrap1 "compile" ctx gcc_jit_context_compile ctx

let set_fields ?(loc = null_loc) struc fields =
  let ctx =
    gcc_jit_object_get_context (gcc_jit_type_as_object (gcc_jit_struct_as_type struc))
  in
  let a = Ctypes.CArray.of_list gcc_jit_field fields in
  wrap3 "set_fields" ctx gcc_jit_struct_set_fields struc loc
    (List.length fields) (Ctypes.CArray.start a)

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

let add_eval ?(loc = null_loc) blk rval =
  let ctx = gcc_jit_object_get_context (gcc_jit_block_as_object blk) in
  wrap3 "add_eval" ctx gcc_jit_block_add_eval blk loc rval

let add_assignment ?(loc = null_loc) blk lval rval =
  let ctx = gcc_jit_object_get_context (gcc_jit_block_as_object blk) in
  wrap4 "add_assignment" ctx gcc_jit_block_add_assignment blk loc lval rval

let add_assignment_op ?(loc = null_loc) blk lval op rval =
  let ctx = gcc_jit_object_get_context (gcc_jit_block_as_object blk) in
  wrap5 "add_assignment_op" ctx gcc_jit_block_add_assignment_op blk loc lval (binary_op op) rval

let add_comment ?(loc = null_loc) blk str =
  let ctx = gcc_jit_object_get_context (gcc_jit_block_as_object blk) in
  wrap3 "add_comment" ctx gcc_jit_block_add_comment blk loc str

let get_function blk =
  let ctx = gcc_jit_object_get_context (gcc_jit_block_as_object blk) in
  wrap1 "get_function" ctx gcc_jit_block_get_function blk
