open Ctypes

type gcc_jit_context
type gcc_jit_result
type gcc_jit_object
type gcc_jit_location
type gcc_jit_type
type gcc_jit_field
type gcc_jit_struct
type gcc_jit_param
type gcc_jit_lvalue
type gcc_jit_rvalue
type gcc_jit_function
type gcc_jit_block

type gcc_jit_str_option =
  | GCC_JIT_STR_OPTION_PROGNAME

type gcc_jit_int_option =
  | GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL

type gcc_jit_bool_option =
  | GCC_JIT_BOOL_OPTION_DEBUGINFO
  | GCC_JIT_BOOL_OPTION_DUMP_INITIAL_TREE
  | GCC_JIT_BOOL_OPTION_DUMP_INITIAL_GIMPLE
  | GCC_JIT_BOOL_OPTION_DUMP_GENERATED_CODE
  | GCC_JIT_BOOL_OPTION_DUMP_SUMMARY
  | GCC_JIT_BOOL_OPTION_DUMP_EVERYTHING
  | GCC_JIT_BOOL_OPTION_KEEP_INTERMEDIATES

type gcc_jit_output_kind =
  | GCC_JIT_OUTPUT_KIND_ASSEMBLER
  | GCC_JIT_OUTPUT_KIND_OBJECT_FILE
  | GCC_JIT_OUTPUT_KIND_DYNAMIC_LIBRARY
  | GCC_JIT_OUTPUT_KIND_EXECUTABLE

type gcc_jit_types =
  | GCC_JIT_TYPE_VOID
  | GCC_JIT_TYPE_VOID_PTR
  | GCC_JIT_TYPE_BOOL
  | GCC_JIT_TYPE_CHAR
  | GCC_JIT_TYPE_SIGNED_CHAR
  | GCC_JIT_TYPE_UNSIGNED_CHAR
  | GCC_JIT_TYPE_SHORT
  | GCC_JIT_TYPE_UNSIGNED_SHORT
  | GCC_JIT_TYPE_INT
  | GCC_JIT_TYPE_UNSIGNED_INT
  | GCC_JIT_TYPE_LONG
  | GCC_JIT_TYPE_UNSIGNED_LONG
  | GCC_JIT_TYPE_LONG_LONG
  | GCC_JIT_TYPE_UNSIGNED_LONG_LONG
  | GCC_JIT_TYPE_FLOAT
  | GCC_JIT_TYPE_DOUBLE
  | GCC_JIT_TYPE_LONG_DOUBLE
  | GCC_JIT_TYPE_CONST_CHAR_PTR
  | GCC_JIT_TYPE_SIZE_T
  | GCC_JIT_TYPE_FILE_PTR
  | GCC_JIT_TYPE_COMPLEX_FLOAT
  | GCC_JIT_TYPE_COMPLEX_DOUBLE
  | GCC_JIT_TYPE_COMPLEX_LONG_DOUBLE

type gcc_jit_function_kind =
  | GCC_JIT_FUNCTION_EXPORTED
  | GCC_JIT_FUNCTION_INTERNAL
  | GCC_JIT_FUNCTION_IMPORTED
  | GCC_JIT_FUNCTION_ALWAYS_INLINE

type gcc_jit_global_kind =
  | GCC_JIT_GLOBAL_EXPORTED
  | GCC_JIT_GLOBAL_INTERNAL
  | GCC_JIT_GLOBAL_IMPORTED

type gcc_jit_unary_op =
  | GCC_JIT_UNARY_OP_MINUS
  | GCC_JIT_UNARY_OP_BITWISE_NEGATE
  | GCC_JIT_UNARY_OP_LOGICAL_NEGATE
  | GCC_JIT_UNARY_OP_ABS

type gcc_jit_binary_op =
  | GCC_JIT_BINARY_OP_PLUS
  | GCC_JIT_BINARY_OP_MINUS
  | GCC_JIT_BINARY_OP_MULT
  | GCC_JIT_BINARY_OP_DIVIDE
  | GCC_JIT_BINARY_OP_MODULO
  | GCC_JIT_BINARY_OP_BITWISE_AND
  | GCC_JIT_BINARY_OP_BITWISE_XOR
  | GCC_JIT_BINARY_OP_BITWISE_OR
  | GCC_JIT_BINARY_OP_LOGICAL_AND
  | GCC_JIT_BINARY_OP_LOGICAL_OR
  | GCC_JIT_BINARY_OP_LSHIFT
  | GCC_JIT_BINARY_OP_RSHIFT

type gcc_jit_comparison =
  | GCC_JIT_COMPARISON_EQ
  | GCC_JIT_COMPARISON_NE
  | GCC_JIT_COMPARISON_LT
  | GCC_JIT_COMPARISON_LE
  | GCC_JIT_COMPARISON_GT
  | GCC_JIT_COMPARISON_GE

let gcc_jit_context : gcc_jit_context structure ptr typ = ptr (structure "gcc_jit_context")
let gcc_jit_result : gcc_jit_result structure ptr typ = ptr (structure "gcc_jit_result")
let gcc_jit_object : gcc_jit_object structure ptr typ = ptr (structure "gcc_jit_object")
let gcc_jit_location : gcc_jit_location structure ptr typ = ptr (structure "gcc_jit_location")
let gcc_jit_type : gcc_jit_type structure ptr typ = ptr (structure "gcc_jit_type")
let gcc_jit_field : gcc_jit_field structure ptr typ = ptr (structure "gcc_jit_field")
let gcc_jit_struct : gcc_jit_struct structure ptr typ = ptr (structure "gcc_jit_struct")
let gcc_jit_param : gcc_jit_param structure ptr typ = ptr (structure "gcc_jit_param")
let gcc_jit_lvalue : gcc_jit_lvalue structure ptr typ = ptr (structure "gcc_jit_lvalue")
let gcc_jit_rvalue : gcc_jit_rvalue structure ptr typ = ptr (structure "gcc_jit_rvalue")
let gcc_jit_function : gcc_jit_function structure ptr typ = ptr (structure "gcc_jit_function")
let gcc_jit_block : gcc_jit_block structure ptr typ = ptr (structure "gcc_jit_block")

module Enums (T : Cstubs_structs.TYPE) = struct

  let gcc_jit_str_option_progname = T.constant "GCC_JIT_STR_OPTION_PROGNAME" T.int64_t

  let gcc_jit_str_option =
    T.enum "gcc_jit_str_option" [ GCC_JIT_STR_OPTION_PROGNAME, gcc_jit_str_option_progname ]

  let gcc_jit_int_option_optimization_level =
    T.constant "GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL" T.int64_t

  let gcc_jit_int_option =
    T.enum "gcc_jit_int_option" [ GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL, gcc_jit_int_option_optimization_level ]

  let gcc_jit_bool_option_debuginfo = T.constant "GCC_JIT_BOOL_OPTION_DEBUGINFO" T.int64_t
  let gcc_jit_bool_option_dump_initial_tree = T.constant "GCC_JIT_BOOL_OPTION_DUMP_INITIAL_TREE" T.int64_t
  let gcc_jit_bool_option_dump_initial_gimple = T.constant "GCC_JIT_BOOL_OPTION_DUMP_INITIAL_GIMPLE" T.int64_t
  let gcc_jit_bool_option_dump_generated_code = T.constant "GCC_JIT_BOOL_OPTION_DUMP_GENERATED_CODE" T.int64_t
  let gcc_jit_bool_option_dump_summary = T.constant "GCC_JIT_BOOL_OPTION_DUMP_SUMMARY" T.int64_t
  let gcc_jit_bool_option_dump_everything = T.constant "GCC_JIT_BOOL_OPTION_DUMP_EVERYTHING" T.int64_t
  let gcc_jit_bool_option_selfcheck_gc = T.constant "GCC_JIT_BOOL_OPTION_SELFCHECK_GC" T.int64_t
  let gcc_jit_bool_option_keep_intermediates = T.constant "GCC_JIT_BOOL_OPTION_KEEP_INTERMEDIATES" T.int64_t

  let gcc_jit_bool_option =
    T.enum "gcc_jit_bool_option"
      [ GCC_JIT_BOOL_OPTION_DEBUGINFO, gcc_jit_bool_option_debuginfo;
        GCC_JIT_BOOL_OPTION_DUMP_INITIAL_TREE, gcc_jit_bool_option_dump_initial_tree;
        GCC_JIT_BOOL_OPTION_DUMP_INITIAL_GIMPLE, gcc_jit_bool_option_dump_initial_gimple;
        GCC_JIT_BOOL_OPTION_DUMP_GENERATED_CODE, gcc_jit_bool_option_dump_generated_code;
        GCC_JIT_BOOL_OPTION_DUMP_SUMMARY, gcc_jit_bool_option_dump_summary;
        GCC_JIT_BOOL_OPTION_DUMP_EVERYTHING, gcc_jit_bool_option_dump_everything;
        GCC_JIT_BOOL_OPTION_KEEP_INTERMEDIATES, gcc_jit_bool_option_keep_intermediates ]

  let gcc_jit_output_kind_assembler = T.constant "GCC_JIT_OUTPUT_KIND_ASSEMBLER" T.int64_t
  let gcc_jit_output_kind_object_file = T.constant "GCC_JIT_OUTPUT_KIND_OBJECT_FILE" T.int64_t
  let gcc_jit_output_kind_dynamic_library = T.constant "GCC_JIT_OUTPUT_KIND_DYNAMIC_LIBRARY" T.int64_t
  let gcc_jit_output_kind_executable = T.constant "GCC_JIT_OUTPUT_KIND_EXECUTABLE" T.int64_t

  let gcc_jit_output_kind =
    T.enum "gcc_jit_output_kind"
      [ GCC_JIT_OUTPUT_KIND_ASSEMBLER, gcc_jit_output_kind_assembler;
        GCC_JIT_OUTPUT_KIND_OBJECT_FILE, gcc_jit_output_kind_object_file;
        GCC_JIT_OUTPUT_KIND_DYNAMIC_LIBRARY, gcc_jit_output_kind_dynamic_library;
        GCC_JIT_OUTPUT_KIND_EXECUTABLE, gcc_jit_output_kind_executable ]

  let gcc_jit_type_void = T.constant "GCC_JIT_TYPE_VOID" T.int64_t
  let gcc_jit_type_void_ptr = T.constant "GCC_JIT_TYPE_VOID_PTR" T.int64_t
  let gcc_jit_type_bool = T.constant "GCC_JIT_TYPE_BOOL" T.int64_t
  let gcc_jit_type_char = T.constant "GCC_JIT_TYPE_CHAR" T.int64_t
  let gcc_jit_type_signed_char = T.constant "GCC_JIT_TYPE_SIGNED_CHAR" T.int64_t
  let gcc_jit_type_unsigned_char = T.constant "GCC_JIT_TYPE_UNSIGNED_CHAR" T.int64_t
  let gcc_jit_type_short = T.constant "GCC_JIT_TYPE_SHORT" T.int64_t
  let gcc_jit_type_unsigned_short = T.constant "GCC_JIT_TYPE_UNSIGNED_SHORT" T.int64_t
  let gcc_jit_type_int = T.constant "GCC_JIT_TYPE_INT" T.int64_t
  let gcc_jit_type_unsigned_int = T.constant "GCC_JIT_TYPE_UNSIGNED_INT" T.int64_t
  let gcc_jit_type_long = T.constant "GCC_JIT_TYPE_LONG" T.int64_t
  let gcc_jit_type_unsigned_long = T.constant "GCC_JIT_TYPE_UNSIGNED_LONG" T.int64_t
  let gcc_jit_type_long_long = T.constant "GCC_JIT_TYPE_LONG_LONG" T.int64_t
  let gcc_jit_type_unsigned_long_long = T.constant "GCC_JIT_TYPE_UNSIGNED_LONG_LONG" T.int64_t
  let gcc_jit_type_float = T.constant "GCC_JIT_TYPE_FLOAT" T.int64_t
  let gcc_jit_type_double = T.constant "GCC_JIT_TYPE_DOUBLE" T.int64_t
  let gcc_jit_type_long_double = T.constant "GCC_JIT_TYPE_LONG_DOUBLE" T.int64_t
  let gcc_jit_type_const_char_ptr = T.constant "GCC_JIT_TYPE_CONST_CHAR_PTR" T.int64_t
  let gcc_jit_type_size_t = T.constant "GCC_JIT_TYPE_SIZE_T" T.int64_t
  let gcc_jit_type_file_ptr = T.constant "GCC_JIT_TYPE_FILE_PTR" T.int64_t
  let gcc_jit_type_complex_float = T.constant "GCC_JIT_TYPE_COMPLEX_FLOAT" T.int64_t
  let gcc_jit_type_complex_double = T.constant "GCC_JIT_TYPE_COMPLEX_DOUBLE" T.int64_t
  let gcc_jit_type_complex_long_double = T.constant "GCC_JIT_TYPE_COMPLEX_LONG_DOUBLE" T.int64_t

  let gcc_jit_types =
    T.enum "gcc_jit_types"
      [ GCC_JIT_TYPE_VOID, gcc_jit_type_void;
        GCC_JIT_TYPE_VOID_PTR, gcc_jit_type_void_ptr;
        GCC_JIT_TYPE_BOOL, gcc_jit_type_bool;
        GCC_JIT_TYPE_CHAR, gcc_jit_type_char;
        GCC_JIT_TYPE_SIGNED_CHAR, gcc_jit_type_signed_char;
        GCC_JIT_TYPE_UNSIGNED_CHAR, gcc_jit_type_unsigned_char;
        GCC_JIT_TYPE_SHORT, gcc_jit_type_short;
        GCC_JIT_TYPE_UNSIGNED_SHORT, gcc_jit_type_unsigned_short;
        GCC_JIT_TYPE_INT, gcc_jit_type_int;
        GCC_JIT_TYPE_UNSIGNED_INT, gcc_jit_type_unsigned_int;
        GCC_JIT_TYPE_LONG, gcc_jit_type_long;
        GCC_JIT_TYPE_UNSIGNED_LONG, gcc_jit_type_unsigned_long;
        GCC_JIT_TYPE_LONG_LONG, gcc_jit_type_long_long;
        GCC_JIT_TYPE_UNSIGNED_LONG_LONG, gcc_jit_type_unsigned_long_long;
        GCC_JIT_TYPE_FLOAT, gcc_jit_type_float;
        GCC_JIT_TYPE_DOUBLE, gcc_jit_type_double;
        GCC_JIT_TYPE_LONG_DOUBLE, gcc_jit_type_long_double;
        GCC_JIT_TYPE_CONST_CHAR_PTR, gcc_jit_type_const_char_ptr;
        GCC_JIT_TYPE_SIZE_T, gcc_jit_type_size_t;
        GCC_JIT_TYPE_FILE_PTR, gcc_jit_type_file_ptr;
        GCC_JIT_TYPE_COMPLEX_FLOAT, gcc_jit_type_complex_float;
        GCC_JIT_TYPE_COMPLEX_DOUBLE, gcc_jit_type_complex_double;
        GCC_JIT_TYPE_COMPLEX_LONG_DOUBLE, gcc_jit_type_complex_long_double ]

  let gcc_jit_function_exported = T.constant "GCC_JIT_FUNCTION_EXPORTED" T.int64_t
  let gcc_jit_function_internal = T.constant "GCC_JIT_FUNCTION_INTERNAL" T.int64_t
  let gcc_jit_function_imported = T.constant "GCC_JIT_FUNCTION_IMPORTED" T.int64_t
  let gcc_jit_function_always_inline = T.constant "GCC_JIT_FUNCTION_ALWAYS_INLINE" T.int64_t

  let gcc_jit_function_kind =
    T.enum "gcc_jit_function_kind"
      [ GCC_JIT_FUNCTION_EXPORTED, gcc_jit_function_exported;
        GCC_JIT_FUNCTION_INTERNAL, gcc_jit_function_internal;
        GCC_JIT_FUNCTION_IMPORTED, gcc_jit_function_imported;
        GCC_JIT_FUNCTION_ALWAYS_INLINE, gcc_jit_function_always_inline ]

  let gcc_jit_global_exported = T.constant "GCC_JIT_GLOBAL_EXPORTED" T.int64_t
  let gcc_jit_global_internal = T.constant "GCC_JIT_GLOBAL_INTERNAL" T.int64_t
  let gcc_jit_global_imported = T.constant "GCC_JIT_GLOBAL_IMPORTED" T.int64_t

  let gcc_jit_global_kind =
    T.enum "gcc_jit_global_kind"
      [ GCC_JIT_GLOBAL_EXPORTED, gcc_jit_global_exported;
        GCC_JIT_GLOBAL_INTERNAL, gcc_jit_global_internal;
        GCC_JIT_GLOBAL_IMPORTED, gcc_jit_global_imported ]

  let gcc_jit_unary_op_minus = T.constant "GCC_JIT_UNARY_OP_MINUS" T.int64_t
  let gcc_jit_unary_op_bitwise_negate = T.constant "GCC_JIT_UNARY_OP_BITWISE_NEGATE" T.int64_t
  let gcc_jit_unary_op_logical_negate = T.constant "GCC_JIT_UNARY_OP_LOGICAL_NEGATE" T.int64_t
  let gcc_jit_unary_op_abs = T.constant "GCC_JIT_UNARY_OP_ABS" T.int64_t

  let gcc_jit_unary_op =
    T.enum "gcc_jit_unary_op"
      [ GCC_JIT_UNARY_OP_MINUS, gcc_jit_unary_op_minus;
        GCC_JIT_UNARY_OP_BITWISE_NEGATE, gcc_jit_unary_op_bitwise_negate;
        GCC_JIT_UNARY_OP_LOGICAL_NEGATE, gcc_jit_unary_op_logical_negate;
        GCC_JIT_UNARY_OP_ABS, gcc_jit_unary_op_abs ]

  let gcc_jit_binary_op_plus = T.constant "GCC_JIT_BINARY_OP_PLUS" T.int64_t
  let gcc_jit_binary_op_minus = T.constant "GCC_JIT_BINARY_OP_MINUS" T.int64_t
  let gcc_jit_binary_op_mult = T.constant "GCC_JIT_BINARY_OP_MULT" T.int64_t
  let gcc_jit_binary_op_divide = T.constant "GCC_JIT_BINARY_OP_DIVIDE" T.int64_t
  let gcc_jit_binary_op_modulo = T.constant "GCC_JIT_BINARY_OP_MODULO" T.int64_t
  let gcc_jit_binary_op_bitwise_and = T.constant "GCC_JIT_BINARY_OP_BITWISE_AND" T.int64_t
  let gcc_jit_binary_op_bitwise_xor = T.constant "GCC_JIT_BINARY_OP_BITWISE_XOR" T.int64_t
  let gcc_jit_binary_op_bitwise_or = T.constant "GCC_JIT_BINARY_OP_BITWISE_OR" T.int64_t
  let gcc_jit_binary_op_logical_and = T.constant "GCC_JIT_BINARY_OP_LOGICAL_AND" T.int64_t
  let gcc_jit_binary_op_logical_or = T.constant "GCC_JIT_BINARY_OP_LOGICAL_OR" T.int64_t
  let gcc_jit_binary_op_lshift = T.constant "GCC_JIT_BINARY_OP_LSHIFT" T.int64_t
  let gcc_jit_binary_op_rshift = T.constant "GCC_JIT_BINARY_OP_RSHIFT" T.int64_t

  let gcc_jit_binary_op =
    T.enum "gcc_jit_binary_op"
      [ GCC_JIT_BINARY_OP_PLUS, gcc_jit_binary_op_plus;
        GCC_JIT_BINARY_OP_MINUS, gcc_jit_binary_op_minus;
        GCC_JIT_BINARY_OP_MULT, gcc_jit_binary_op_mult;
        GCC_JIT_BINARY_OP_DIVIDE, gcc_jit_binary_op_divide;
        GCC_JIT_BINARY_OP_MODULO, gcc_jit_binary_op_modulo;
        GCC_JIT_BINARY_OP_BITWISE_AND, gcc_jit_binary_op_bitwise_and;
        GCC_JIT_BINARY_OP_BITWISE_XOR, gcc_jit_binary_op_bitwise_xor;
        GCC_JIT_BINARY_OP_BITWISE_OR, gcc_jit_binary_op_bitwise_or;
        GCC_JIT_BINARY_OP_LOGICAL_AND, gcc_jit_binary_op_logical_and;
        GCC_JIT_BINARY_OP_LOGICAL_OR, gcc_jit_binary_op_logical_or;
        GCC_JIT_BINARY_OP_LSHIFT, gcc_jit_binary_op_lshift;
        GCC_JIT_BINARY_OP_RSHIFT, gcc_jit_binary_op_rshift ]

  let gcc_jit_comparison_eq = T.constant "GCC_JIT_COMPARISON_EQ" T.int64_t
  let gcc_jit_comparison_ne = T.constant "GCC_JIT_COMPARISON_NE" T.int64_t
  let gcc_jit_comparison_lt = T.constant "GCC_JIT_COMPARISON_LT" T.int64_t
  let gcc_jit_comparison_le = T.constant "GCC_JIT_COMPARISON_LE" T.int64_t
  let gcc_jit_comparison_gt = T.constant "GCC_JIT_COMPARISON_GT" T.int64_t
  let gcc_jit_comparison_ge = T.constant "GCC_JIT_COMPARISON_GE" T.int64_t

  let gcc_jit_comparison =
    T.enum "gcc_jit_comparison"
      [ GCC_JIT_COMPARISON_EQ, gcc_jit_comparison_eq;
        GCC_JIT_COMPARISON_NE, gcc_jit_comparison_ne;
        GCC_JIT_COMPARISON_LT, gcc_jit_comparison_lt;
        GCC_JIT_COMPARISON_LE, gcc_jit_comparison_le;
        GCC_JIT_COMPARISON_GT, gcc_jit_comparison_gt;
        GCC_JIT_COMPARISON_GE, gcc_jit_comparison_ge ]

end

module Bindings (T : Cstubs_structs.TYPE with type 'a typ = 'a typ) (F : Cstubs.FOREIGN) = struct

  module E = Enums (T)

  let gcc_jit_context_acquire =
    F.foreign "gcc_jit_context_acquire" (void @-> returning gcc_jit_context)

  let gcc_jit_context_release =
    F.foreign "gcc_jit_context_release" (gcc_jit_context @-> returning void)

  let gcc_jit_context_set_str_option =
    F.foreign "gcc_jit_context_set_str_option"
      (gcc_jit_context @-> E.gcc_jit_str_option @-> string @-> returning void)

  let gcc_jit_context_set_int_option =
    F.foreign "gcc_jit_context_set_int_option"
      (gcc_jit_context @-> E.gcc_jit_int_option @-> int @-> returning void)

  let gcc_jit_context_set_bool_option =
    F.foreign "gcc_jit_context_set_bool_option"
      (gcc_jit_context @-> E.gcc_jit_bool_option @-> bool @-> returning void)

  let gcc_jit_context_compile =
    F.foreign "gcc_jit_context_compile" (gcc_jit_context @-> returning gcc_jit_result)

  let gcc_jit_context_compile_to_file =
    F.foreign "gcc_jit_context_compile_to_file"
      (gcc_jit_context @-> E.gcc_jit_output_kind @-> string @-> returning void)

  let gcc_jit_context_dump_to_file =
    F.foreign "gcc_jit_context_dump_to_file"
      (gcc_jit_context @-> string @-> int @-> returning void)

  let gcc_jit_context_set_logfile =
    F.foreign "gcc_jit_context_set_logfile"
      (gcc_jit_context @-> ptr void @-> int @-> int @-> returning void)

  let gcc_jit_context_get_first_error =
    F.foreign "gcc_jit_context_get_first_error" (gcc_jit_context @-> returning string_opt)

  let gcc_jit_context_get_last_error =
    F.foreign "gcc_jit_context_get_last_error" (gcc_jit_context @-> returning string_opt)

  let gcc_jit_result_get_code =
    F.foreign "gcc_jit_result_get_code" (gcc_jit_result @-> string @-> returning (ptr void))

  let gcc_jit_get_global =
    F.foreign "gcc_jit_get_global" (gcc_jit_result @-> string @-> returning (ptr void))

  let gcc_jit_result_release =
    F.foreign "gcc_jit_result_release" (gcc_jit_result @-> returning void)

  let gcc_jit_object_get_context =
    F.foreign "gcc_jit_object_get_context" (gcc_jit_object @-> returning gcc_jit_context)

  let gcc_jit_object_get_debug_string =
    F.foreign "gcc_jit_object_get_debug_string" (gcc_jit_object @-> returning string) (* CHECK string NULL ? *)

  let gcc_jit_context_new_location =
    F.foreign "gcc_jit_context_new_location"
      (gcc_jit_context @-> string @-> int @-> int @-> returning gcc_jit_location)

  let gcc_jit_location_as_object =
    F.foreign "gcc_jit_location_as_object" (gcc_jit_location @-> returning gcc_jit_object)

  let gcc_jit_type_as_object =
    F.foreign "gcc_jit_type_as_object" (gcc_jit_type @-> returning gcc_jit_object)

  let gcc_jit_context_get_type =
    F.foreign "gcc_jit_context_get_type"
      (gcc_jit_context @-> E.gcc_jit_types @-> returning gcc_jit_type)

  let gcc_jit_context_get_int_type =
    F.foreign "gcc_jit_context_get_int_type"
      (gcc_jit_context @-> int @-> int @-> returning gcc_jit_type)

  let gcc_jit_type_get_pointer =
    F.foreign "gcc_jit_type_get_pointer" (gcc_jit_type @-> returning gcc_jit_type)

  let gcc_jit_type_get_const =
    F.foreign "gcc_jit_type_get_const" (gcc_jit_type @-> returning gcc_jit_type)

  let gcc_jit_type_get_volatile =
    F.foreign "gcc_jit_type_get_volatile" (gcc_jit_type @-> returning gcc_jit_type)

  let gcc_jit_context_new_array_type =
    F.foreign "gcc_jit_context_new_array_type"
      (gcc_jit_context @-> gcc_jit_location @-> gcc_jit_type @-> int @-> returning gcc_jit_type)

  let gcc_jit_context_new_field =
    F.foreign "gcc_jit_context_new_field"
      (gcc_jit_context @-> gcc_jit_location @-> gcc_jit_type @-> string @-> returning gcc_jit_field)

  let gcc_jit_field_as_object =
    F.foreign "gcc_jit_field_as_object" (gcc_jit_field @-> returning gcc_jit_object)

  let gcc_jit_context_new_struct_type =
    F.foreign "gcc_jit_context_new_struct_type"
      (gcc_jit_context @-> gcc_jit_location @-> string @-> int @-> ptr gcc_jit_field @-> returning gcc_jit_struct)

  let gcc_jit_context_new_opaque_struct =
    F.foreign "gcc_jit_context_new_opaque_struct"
      (gcc_jit_context @-> gcc_jit_location @-> string @-> returning gcc_jit_struct)

  let gcc_jit_struct_as_type =
    F.foreign "gcc_jit_struct_as_type" (gcc_jit_struct @-> returning gcc_jit_type)

  let gcc_jit_struct_set_fields =
    F.foreign "gcc_jit_struct_set_fields"
      (gcc_jit_struct @-> gcc_jit_location @-> int @-> ptr gcc_jit_field @-> returning void)

  let gcc_jit_context_new_union_type =
    F.foreign "gcc_jit_context_new_union_type"
      (gcc_jit_context @-> gcc_jit_location @-> string @-> int @-> ptr gcc_jit_field @-> returning gcc_jit_type)

  let gcc_jit_context_new_function_ptr_type =
    F.foreign "gcc_jit_context_new_function_ptr_type"
      (gcc_jit_context @-> gcc_jit_location @-> gcc_jit_type @-> int @-> ptr gcc_jit_type @-> int @->
       returning gcc_jit_type)

  let gcc_jit_context_new_param =
    F.foreign "gcc_jit_context_new_param"
      (gcc_jit_context @-> gcc_jit_location @-> gcc_jit_type @-> string @-> returning gcc_jit_param)

  let gcc_jit_param_as_object =
    F.foreign "gcc_jit_param_as_object" (gcc_jit_param @-> returning gcc_jit_object)

  let gcc_jit_param_as_lvalue =
    F.foreign "gcc_jit_param_as_lvalue" (gcc_jit_param @-> returning gcc_jit_lvalue)

  let gcc_jit_param_as_rvalue =
    F.foreign "gcc_jit_param_as_rvalue" (gcc_jit_param @-> returning gcc_jit_rvalue)

  let gcc_jit_context_new_function =
    F.foreign "gcc_jit_context_new_function"
      (gcc_jit_context @-> gcc_jit_location @-> E.gcc_jit_function_kind @->
       gcc_jit_type @-> string @-> int @-> ptr gcc_jit_param @-> int @-> returning gcc_jit_function)

  let gcc_jit_context_get_builtin_function =
    F.foreign "gcc_jit_context_get_builtin_function"
      (gcc_jit_context @-> string @-> returning gcc_jit_function)

  let gcc_jit_function_as_object =
    F.foreign "gcc_jit_function_as_object" (gcc_jit_function @-> returning gcc_jit_object)

  let gcc_jit_function_get_param =
    F.foreign "gcc_jit_function_get_param" (gcc_jit_function @-> int @-> returning gcc_jit_param)

  let gcc_jit_function_dump_to_dot =
    F.foreign "gcc_jit_function_dump_to_dot" (gcc_jit_function @-> string @-> returning void)

  let gcc_jit_function_new_block =
    F.foreign "gcc_jit_functin_new_block" (gcc_jit_function @-> string @-> returning gcc_jit_block)

  let gcc_jit_block_as_object =
    F.foreign "gcc_jit_block_as_object" (gcc_jit_block @-> returning gcc_jit_object)

  let gcc_jit_block_get_function =
    F.foreign "gcc_jit_block_get_function" (gcc_jit_block @-> returning gcc_jit_function)

  let gcc_jit_context_new_global =
    F.foreign "gcc_jit_context_new_global"
      (gcc_jit_context @-> gcc_jit_location @-> E.gcc_jit_global_kind @-> gcc_jit_type @-> string @->
       returning gcc_jit_lvalue)

  let gcc_jit_lvalue_as_object =
    F.foreign "gcc_jit_lvalue_as_object" (gcc_jit_lvalue @-> returning gcc_jit_object)

  let gcc_jit_lvalue_as_rvalue =
    F.foreign "gcc_jit_lvalue_as_rvalue" (gcc_jit_lvalue @-> returning gcc_jit_rvalue)

  let gcc_jit_rvalue_as_object =
    F.foreign "gcc_jit_rvalue_as_object" (gcc_jit_rvalue @-> returning gcc_jit_object)

  let gcc_jit_rvalue_get_type =
    F.foreign "gcc_jit_rvalue_get_type" (gcc_jit_rvalue @-> returning gcc_jit_type)

  let gcc_jit_context_new_rvalue_from_int =
    F.foreign "gcc_jit_context_new_rvalue_from_int"
      (gcc_jit_context @-> gcc_jit_type @-> int @-> returning gcc_jit_rvalue) (* CHECK int *)

  let gcc_jit_context_new_rvalue_from_long =
    F.foreign "gcc_jit_context_new_rvalue_from_long"
      (gcc_jit_context @-> gcc_jit_type @-> int @-> returning gcc_jit_rvalue) (* CHECK int *)

  let gcc_jit_context_zero =
    F.foreign "gcc_jit_context_zero" (gcc_jit_context @-> gcc_jit_type @-> returning gcc_jit_rvalue)

  let gcc_jit_context_one =
    F.foreign "gcc_jit_context_one" (gcc_jit_context @-> gcc_jit_type @-> returning gcc_jit_rvalue)

  let gcc_jit_context_new_rvalue_from_double =
    F.foreign "gcc_jit_context_new_rvalue_from_double"
      (gcc_jit_context @-> gcc_jit_type @-> float @-> returning gcc_jit_rvalue)

  let gcc_jit_context_new_rvalue_from_ptr =
    F.foreign "gcc_jit_context_new_rvalue_from_ptr"
      (gcc_jit_context @-> gcc_jit_type @-> ptr void @-> returning gcc_jit_rvalue)

  let gcc_jit_context_null =
    F.foreign "gcc_jit_context_null" (gcc_jit_context @-> gcc_jit_type @-> returning gcc_jit_rvalue)

  let gcc_jit_context_new_string_literal =
    F.foreign "gcc_jit_context_new_string_literal" (gcc_jit_context @-> string @-> returning gcc_jit_rvalue)

  let gcc_jit_context_new_unary_op =
    F.foreign "gcc_jit_context_new_unary_op"
      (gcc_jit_context @-> gcc_jit_location @-> E.gcc_jit_unary_op @-> gcc_jit_type @-> gcc_jit_rvalue @->
       returning gcc_jit_rvalue)

  let gcc_jit_context_new_binary_op =
    F.foreign "gcc_jit_context_new_binary_op"
      (gcc_jit_context @-> gcc_jit_location @-> E.gcc_jit_binary_op @-> gcc_jit_type @-> gcc_jit_rvalue @->
       gcc_jit_rvalue @-> returning gcc_jit_rvalue)

  let gcc_jit_context_new_comparison =
    F.foreign "gcc_jit_context_new_comparison"
      (gcc_jit_context @-> gcc_jit_location @-> E.gcc_jit_comparison @-> gcc_jit_rvalue @-> gcc_jit_rvalue @->
       returning gcc_jit_rvalue)

  let gcc_jit_context_new_call =
    F.foreign "gcc_jit_context_new_call"
      (gcc_jit_context @-> gcc_jit_location @-> gcc_jit_function @-> int @-> ptr gcc_jit_rvalue @->
       returning gcc_jit_rvalue)

  let gcc_jit_context_new_call_through_ptr =
    F.foreign "gcc_jit_context_new_call_through_ptr"
      (gcc_jit_context @-> gcc_jit_location @-> gcc_jit_rvalue @-> int @-> ptr gcc_jit_rvalue @->
       returning gcc_jit_rvalue)

  let gcc_jit_context_new_cast =
    F.foreign "gcc_jit_context_new_cast"
      (gcc_jit_context @-> gcc_jit_location @-> gcc_jit_rvalue @-> gcc_jit_type @-> returning gcc_jit_rvalue)

  let gcc_jit_context_new_array_access =
    F.foreign "gcc_jit_context_new_array_access"
      (gcc_jit_context @-> gcc_jit_location @-> gcc_jit_rvalue @-> gcc_jit_rvalue @-> returning gcc_jit_lvalue)

  let gcc_jit_lvalue_access_field =
    F.foreign "gcc_jit_lvalue_access_field"
      (gcc_jit_lvalue @-> gcc_jit_location @-> gcc_jit_field @-> returning gcc_jit_lvalue)

  let gcc_jit_rvalue_access_field =
    F.foreign "gcc_jit_rvalue_access_field"
      (gcc_jit_rvalue @-> gcc_jit_location @-> gcc_jit_field @-> returning gcc_jit_rvalue)

  let gcc_jit_rvalue_dereference_field =
    F.foreign "gcc_jit_rvalue_dereference_field"
      (gcc_jit_rvalue @-> gcc_jit_location @-> gcc_jit_field @-> returning gcc_jit_lvalue)

  let gcc_jit_rvalue_dereference =
    F.foreign "gcc_jit_rvalue_dereference"
      (gcc_jit_rvalue @-> gcc_jit_location @-> returning gcc_jit_lvalue)

  let gcc_jit_lvalue_get_address =
    F.foreign "gcc_jit_lvalue_get_address"
      (gcc_jit_lvalue @-> gcc_jit_location @-> returning gcc_jit_rvalue)

  let gcc_jit_function_new_local =
    F.foreign "gcc_jit_function_new_local"
      (gcc_jit_function @-> gcc_jit_location @-> gcc_jit_type @-> string @-> returning gcc_jit_lvalue)

  let gcc_jit_block_add_eval =
    F.foreign "gcc_jit_block_add_eval"
      (gcc_jit_block @-> gcc_jit_location @-> gcc_jit_rvalue @-> returning void)

  let gcc_jit_block_add_assignment =
    F.foreign "gcc_jit_block_add_assignment"
      (gcc_jit_block @-> gcc_jit_location @-> gcc_jit_lvalue @-> gcc_jit_rvalue @-> returning void)

  let gcc_jit_block_add_assignment_op =
    F.foreign "gcc_jit_block_add_assignment_op"
      (gcc_jit_block @-> gcc_jit_location @-> gcc_jit_lvalue @-> E.gcc_jit_binary_op @-> gcc_jit_rvalue @->
       returning void)

  let gcc_jit_block_add_comment =
    F.foreign "gcc_jit_block_add_comment" (gcc_jit_block @-> gcc_jit_location @-> string @-> returning void)

  let gcc_jit_block_end_with_conditional =
    F.foreign "gcc_jit_block_end_with_conditional"
      (gcc_jit_block @-> gcc_jit_location @-> gcc_jit_rvalue @-> gcc_jit_block @-> gcc_jit_block @->
       returning void)

  let gcc_jit_block_end_with_jump =
    F.foreign "gcc_jit_block_end_with_jump"
      (gcc_jit_block @-> gcc_jit_location @-> gcc_jit_block @-> returning void)

  let gcc_jit_block_end_with_return =
    F.foreign "gcc_jit_block_end_with_return"
      (gcc_jit_block @-> gcc_jit_location @-> gcc_jit_rvalue @-> returning void)

  let gcc_jit_block_end_with_void_return =
    F.foreign "gcc_jit_block_end_with_void_return"
      (gcc_jit_block @-> gcc_jit_location @-> returning void)

  let gcc_jit_context_new_child_context =
    F.foreign "gcc_jit_context_new_child_context" (gcc_jit_context @-> returning gcc_jit_context)

  let gcc_jit_context_dump_reproducer_to_file =
    F.foreign "gcc_jit_context_dump_reproducer_to_file" (gcc_jit_context @-> string @-> returning void)

  (* let gcc_jit_context_enable_dump = *)
  (*   F.foreign "gcc_jit_context_enable_dump" (gcc_jit_context @-> string @-> ptr char @-> returning void) *)
end
