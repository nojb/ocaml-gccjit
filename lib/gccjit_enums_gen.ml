include Ctypes
let lift x = x
open Ctypes_static

let field : type a s. 't typ -> string -> a typ ->
  (a, ((s, [<`Struct | `Union]) structured as 't)) field =
  fun (type k) (s : (_, k) structured typ) fname ftype -> match s, fname with
  | _ -> failwith ("Unexpected field "^ fname)

let seal (type t) (t : (_, t) structured typ) = match t with
  | Struct { tag; spec = Complete _; } ->
    raise (ModifyingSealedType tag)
  | Union { utag; uspec = Some _; } ->
    raise (ModifyingSealedType utag)
  | _ ->
    raise (Unsupported "Sealing a non-structured type")
type 'a const = 'a
let constant (type t) name (t : t typ) : t = match name, t with
  | "GCC_JIT_COMPARISON_GE", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    5L
  | "GCC_JIT_COMPARISON_GT", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    4L
  | "GCC_JIT_COMPARISON_LE", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    3L
  | "GCC_JIT_COMPARISON_LT", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    2L
  | "GCC_JIT_COMPARISON_NE", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    1L
  | "GCC_JIT_COMPARISON_EQ", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    0L
  | "GCC_JIT_BINARY_OP_RSHIFT", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    11L
  | "GCC_JIT_BINARY_OP_LSHIFT", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    10L
  | "GCC_JIT_BINARY_OP_LOGICAL_OR", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    9L
  | "GCC_JIT_BINARY_OP_LOGICAL_AND", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    8L
  | "GCC_JIT_BINARY_OP_BITWISE_OR", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    7L
  | "GCC_JIT_BINARY_OP_BITWISE_XOR", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    6L
  | "GCC_JIT_BINARY_OP_BITWISE_AND", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    5L
  | "GCC_JIT_BINARY_OP_MODULO", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    4L
  | "GCC_JIT_BINARY_OP_DIVIDE", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    3L
  | "GCC_JIT_BINARY_OP_MULT", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    2L
  | "GCC_JIT_BINARY_OP_MINUS", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    1L
  | "GCC_JIT_BINARY_OP_PLUS", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    0L
  | "GCC_JIT_UNARY_OP_ABS", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    3L
  | "GCC_JIT_UNARY_OP_LOGICAL_NEGATE", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    2L
  | "GCC_JIT_UNARY_OP_BITWISE_NEGATE", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    1L
  | "GCC_JIT_UNARY_OP_MINUS", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    0L
  | "GCC_JIT_GLOBAL_IMPORTED", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    2L
  | "GCC_JIT_GLOBAL_INTERNAL", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    1L
  | "GCC_JIT_GLOBAL_EXPORTED", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    0L
  | "GCC_JIT_FUNCTION_ALWAYS_INLINE", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    3L
  | "GCC_JIT_FUNCTION_IMPORTED", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    2L
  | "GCC_JIT_FUNCTION_INTERNAL", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    1L
  | "GCC_JIT_FUNCTION_EXPORTED", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    0L
  | "GCC_JIT_TYPE_COMPLEX_LONG_DOUBLE", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    22L
  | "GCC_JIT_TYPE_COMPLEX_DOUBLE", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    21L
  | "GCC_JIT_TYPE_COMPLEX_FLOAT", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    20L
  | "GCC_JIT_TYPE_FILE_PTR", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    19L
  | "GCC_JIT_TYPE_SIZE_T", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    18L
  | "GCC_JIT_TYPE_CONST_CHAR_PTR", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    17L
  | "GCC_JIT_TYPE_LONG_DOUBLE", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    16L
  | "GCC_JIT_TYPE_DOUBLE", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    15L
  | "GCC_JIT_TYPE_FLOAT", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    14L
  | "GCC_JIT_TYPE_UNSIGNED_LONG_LONG", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    13L
  | "GCC_JIT_TYPE_LONG_LONG", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    12L
  | "GCC_JIT_TYPE_UNSIGNED_LONG", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    11L
  | "GCC_JIT_TYPE_LONG", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    10L
  | "GCC_JIT_TYPE_UNSIGNED_INT", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    9L
  | "GCC_JIT_TYPE_INT", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    8L
  | "GCC_JIT_TYPE_UNSIGNED_SHORT", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    7L
  | "GCC_JIT_TYPE_SHORT", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    6L
  | "GCC_JIT_TYPE_UNSIGNED_CHAR", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    5L
  | "GCC_JIT_TYPE_SIGNED_CHAR", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    4L
  | "GCC_JIT_TYPE_CHAR", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    3L
  | "GCC_JIT_TYPE_BOOL", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    2L
  | "GCC_JIT_TYPE_VOID_PTR", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    1L
  | "GCC_JIT_TYPE_VOID", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    0L
  | "GCC_JIT_OUTPUT_KIND_EXECUTABLE", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    3L
  | "GCC_JIT_OUTPUT_KIND_DYNAMIC_LIBRARY", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    2L
  | "GCC_JIT_OUTPUT_KIND_OBJECT_FILE", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    1L
  | "GCC_JIT_OUTPUT_KIND_ASSEMBLER", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    0L
  | "GCC_JIT_BOOL_OPTION_KEEP_INTERMEDIATES", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    7L
  | "GCC_JIT_BOOL_OPTION_SELFCHECK_GC", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    6L
  | "GCC_JIT_BOOL_OPTION_DUMP_EVERYTHING", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    5L
  | "GCC_JIT_BOOL_OPTION_DUMP_SUMMARY", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    4L
  | "GCC_JIT_BOOL_OPTION_DUMP_GENERATED_CODE", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    3L
  | "GCC_JIT_BOOL_OPTION_DUMP_INITIAL_GIMPLE", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    2L
  | "GCC_JIT_BOOL_OPTION_DUMP_INITIAL_TREE", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    1L
  | "GCC_JIT_BOOL_OPTION_DEBUGINFO", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    0L
  | "GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    0L
  | "GCC_JIT_STR_OPTION_PROGNAME", Ctypes_static.Primitive Cstubs_internals.Int64_t ->
    0L
  | s, _ -> failwith ("unmatched constant: "^ s)

let enum (type a) name ?unexpected (alist : (a * int64) list) =
  match name with
  | "gcc_jit_comparison" -> 
    Cstubs_internals.build_enum_type "gcc_jit_comparison" Ctypes_static.Uint32 ?unexpected alist
  | "gcc_jit_binary_op" -> 
    Cstubs_internals.build_enum_type "gcc_jit_binary_op" Ctypes_static.Uint32 ?unexpected alist
  | "gcc_jit_unary_op" -> 
    Cstubs_internals.build_enum_type "gcc_jit_unary_op" Ctypes_static.Uint32 ?unexpected alist
  | "gcc_jit_global_kind" -> 
    Cstubs_internals.build_enum_type "gcc_jit_global_kind" Ctypes_static.Uint32 ?unexpected alist
  | "gcc_jit_function_kind" -> 
    Cstubs_internals.build_enum_type "gcc_jit_function_kind" Ctypes_static.Uint32 ?unexpected alist
  | "gcc_jit_types" -> 
    Cstubs_internals.build_enum_type "gcc_jit_types" Ctypes_static.Uint32 ?unexpected alist
  | "gcc_jit_output_kind" -> 
    Cstubs_internals.build_enum_type "gcc_jit_output_kind" Ctypes_static.Uint32 ?unexpected alist
  | "gcc_jit_bool_option" -> 
    Cstubs_internals.build_enum_type "gcc_jit_bool_option" Ctypes_static.Uint32 ?unexpected alist
  | "gcc_jit_int_option" -> 
    Cstubs_internals.build_enum_type "gcc_jit_int_option" Ctypes_static.Uint32 ?unexpected alist
  | "gcc_jit_str_option" -> 
    Cstubs_internals.build_enum_type "gcc_jit_str_option" Ctypes_static.Uint32 ?unexpected alist
  | s ->
    failwith ("unmatched enum: "^ s)
