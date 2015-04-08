#include <libgccjit.h>
#if !__USE_MINGW_ANSI_STDIO && (defined(__MINGW32__) || defined(__MINGW64__))
#define __USE_MINGW_ANSI_STDIO 1
#endif

#include <stdio.h>
#include <stddef.h>
#include "ctypes_cstubs_internals.h"

int main(void)
{

  puts("include Ctypes");
  puts("let lift x = x");
  puts("open Ctypes_static");
  puts("");
  puts("let field : type a s. 't typ -> string -> a typ ->");
  puts("  (a, ((s, [<`Struct | `Union]) structured as 't)) field =");
  puts("  fun (type k) (s : (_, k) structured typ) fname ftype -> match s, fname with");
  puts("  | _ -> failwith (\"Unexpected field \"^ fname)");
  puts("");
  puts("let seal (type t) (t : (_, t) structured typ) = match t with");
  puts("  | Struct { tag; spec = Complete _; } ->");
  puts("    raise (ModifyingSealedType tag)");
  puts("  | Union { utag; uspec = Some _; } ->");
  puts("    raise (ModifyingSealedType utag)");
  puts("  | _ ->");
  puts("    raise (Unsupported \"Sealing a non-structured type\")");
  puts("type 'a const = 'a");
  puts("let constant (type t) name (t : t typ) : t = match name, t with");
  {
     enum { check_GCC_JIT_COMPARISON_GE_const = (int)GCC_JIT_COMPARISON_GE };
     int64_t v = (GCC_JIT_COMPARISON_GE);
     printf("  | \"GCC_JIT_COMPARISON_GE\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_COMPARISON_GT_const = (int)GCC_JIT_COMPARISON_GT };
     int64_t v = (GCC_JIT_COMPARISON_GT);
     printf("  | \"GCC_JIT_COMPARISON_GT\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_COMPARISON_LE_const = (int)GCC_JIT_COMPARISON_LE };
     int64_t v = (GCC_JIT_COMPARISON_LE);
     printf("  | \"GCC_JIT_COMPARISON_LE\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_COMPARISON_LT_const = (int)GCC_JIT_COMPARISON_LT };
     int64_t v = (GCC_JIT_COMPARISON_LT);
     printf("  | \"GCC_JIT_COMPARISON_LT\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_COMPARISON_NE_const = (int)GCC_JIT_COMPARISON_NE };
     int64_t v = (GCC_JIT_COMPARISON_NE);
     printf("  | \"GCC_JIT_COMPARISON_NE\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_COMPARISON_EQ_const = (int)GCC_JIT_COMPARISON_EQ };
     int64_t v = (GCC_JIT_COMPARISON_EQ);
     printf("  | \"GCC_JIT_COMPARISON_EQ\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_BINARY_OP_RSHIFT_const = (int)GCC_JIT_BINARY_OP_RSHIFT };
     int64_t v = (GCC_JIT_BINARY_OP_RSHIFT);
     printf("  | \"GCC_JIT_BINARY_OP_RSHIFT\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_BINARY_OP_LSHIFT_const = (int)GCC_JIT_BINARY_OP_LSHIFT };
     int64_t v = (GCC_JIT_BINARY_OP_LSHIFT);
     printf("  | \"GCC_JIT_BINARY_OP_LSHIFT\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_BINARY_OP_LOGICAL_OR_const = (int)GCC_JIT_BINARY_OP_LOGICAL_OR };
     int64_t v = (GCC_JIT_BINARY_OP_LOGICAL_OR);
     printf("  | \"GCC_JIT_BINARY_OP_LOGICAL_OR\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_BINARY_OP_LOGICAL_AND_const = (int)GCC_JIT_BINARY_OP_LOGICAL_AND };
     int64_t v = (GCC_JIT_BINARY_OP_LOGICAL_AND);
     printf("  | \"GCC_JIT_BINARY_OP_LOGICAL_AND\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_BINARY_OP_BITWISE_OR_const = (int)GCC_JIT_BINARY_OP_BITWISE_OR };
     int64_t v = (GCC_JIT_BINARY_OP_BITWISE_OR);
     printf("  | \"GCC_JIT_BINARY_OP_BITWISE_OR\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_BINARY_OP_BITWISE_XOR_const = (int)GCC_JIT_BINARY_OP_BITWISE_XOR };
     int64_t v = (GCC_JIT_BINARY_OP_BITWISE_XOR);
     printf("  | \"GCC_JIT_BINARY_OP_BITWISE_XOR\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_BINARY_OP_BITWISE_AND_const = (int)GCC_JIT_BINARY_OP_BITWISE_AND };
     int64_t v = (GCC_JIT_BINARY_OP_BITWISE_AND);
     printf("  | \"GCC_JIT_BINARY_OP_BITWISE_AND\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_BINARY_OP_MODULO_const = (int)GCC_JIT_BINARY_OP_MODULO };
     int64_t v = (GCC_JIT_BINARY_OP_MODULO);
     printf("  | \"GCC_JIT_BINARY_OP_MODULO\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_BINARY_OP_DIVIDE_const = (int)GCC_JIT_BINARY_OP_DIVIDE };
     int64_t v = (GCC_JIT_BINARY_OP_DIVIDE);
     printf("  | \"GCC_JIT_BINARY_OP_DIVIDE\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_BINARY_OP_MULT_const = (int)GCC_JIT_BINARY_OP_MULT };
     int64_t v = (GCC_JIT_BINARY_OP_MULT);
     printf("  | \"GCC_JIT_BINARY_OP_MULT\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_BINARY_OP_MINUS_const = (int)GCC_JIT_BINARY_OP_MINUS };
     int64_t v = (GCC_JIT_BINARY_OP_MINUS);
     printf("  | \"GCC_JIT_BINARY_OP_MINUS\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_BINARY_OP_PLUS_const = (int)GCC_JIT_BINARY_OP_PLUS };
     int64_t v = (GCC_JIT_BINARY_OP_PLUS);
     printf("  | \"GCC_JIT_BINARY_OP_PLUS\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_UNARY_OP_ABS_const = (int)GCC_JIT_UNARY_OP_ABS };
     int64_t v = (GCC_JIT_UNARY_OP_ABS);
     printf("  | \"GCC_JIT_UNARY_OP_ABS\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_UNARY_OP_LOGICAL_NEGATE_const = (int)GCC_JIT_UNARY_OP_LOGICAL_NEGATE };
     int64_t v = (GCC_JIT_UNARY_OP_LOGICAL_NEGATE);
     printf("  | \"GCC_JIT_UNARY_OP_LOGICAL_NEGATE\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_UNARY_OP_BITWISE_NEGATE_const = (int)GCC_JIT_UNARY_OP_BITWISE_NEGATE };
     int64_t v = (GCC_JIT_UNARY_OP_BITWISE_NEGATE);
     printf("  | \"GCC_JIT_UNARY_OP_BITWISE_NEGATE\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_UNARY_OP_MINUS_const = (int)GCC_JIT_UNARY_OP_MINUS };
     int64_t v = (GCC_JIT_UNARY_OP_MINUS);
     printf("  | \"GCC_JIT_UNARY_OP_MINUS\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_GLOBAL_IMPORTED_const = (int)GCC_JIT_GLOBAL_IMPORTED };
     int64_t v = (GCC_JIT_GLOBAL_IMPORTED);
     printf("  | \"GCC_JIT_GLOBAL_IMPORTED\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_GLOBAL_INTERNAL_const = (int)GCC_JIT_GLOBAL_INTERNAL };
     int64_t v = (GCC_JIT_GLOBAL_INTERNAL);
     printf("  | \"GCC_JIT_GLOBAL_INTERNAL\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_GLOBAL_EXPORTED_const = (int)GCC_JIT_GLOBAL_EXPORTED };
     int64_t v = (GCC_JIT_GLOBAL_EXPORTED);
     printf("  | \"GCC_JIT_GLOBAL_EXPORTED\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_FUNCTION_ALWAYS_INLINE_const = (int)GCC_JIT_FUNCTION_ALWAYS_INLINE };
     int64_t v = (GCC_JIT_FUNCTION_ALWAYS_INLINE);
     printf("  | \"GCC_JIT_FUNCTION_ALWAYS_INLINE\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_FUNCTION_IMPORTED_const = (int)GCC_JIT_FUNCTION_IMPORTED };
     int64_t v = (GCC_JIT_FUNCTION_IMPORTED);
     printf("  | \"GCC_JIT_FUNCTION_IMPORTED\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_FUNCTION_INTERNAL_const = (int)GCC_JIT_FUNCTION_INTERNAL };
     int64_t v = (GCC_JIT_FUNCTION_INTERNAL);
     printf("  | \"GCC_JIT_FUNCTION_INTERNAL\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_FUNCTION_EXPORTED_const = (int)GCC_JIT_FUNCTION_EXPORTED };
     int64_t v = (GCC_JIT_FUNCTION_EXPORTED);
     printf("  | \"GCC_JIT_FUNCTION_EXPORTED\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_TYPE_COMPLEX_LONG_DOUBLE_const = (int)GCC_JIT_TYPE_COMPLEX_LONG_DOUBLE };
     int64_t v = (GCC_JIT_TYPE_COMPLEX_LONG_DOUBLE);
     printf("  | \"GCC_JIT_TYPE_COMPLEX_LONG_DOUBLE\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_TYPE_COMPLEX_DOUBLE_const = (int)GCC_JIT_TYPE_COMPLEX_DOUBLE };
     int64_t v = (GCC_JIT_TYPE_COMPLEX_DOUBLE);
     printf("  | \"GCC_JIT_TYPE_COMPLEX_DOUBLE\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_TYPE_COMPLEX_FLOAT_const = (int)GCC_JIT_TYPE_COMPLEX_FLOAT };
     int64_t v = (GCC_JIT_TYPE_COMPLEX_FLOAT);
     printf("  | \"GCC_JIT_TYPE_COMPLEX_FLOAT\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_TYPE_FILE_PTR_const = (int)GCC_JIT_TYPE_FILE_PTR };
     int64_t v = (GCC_JIT_TYPE_FILE_PTR);
     printf("  | \"GCC_JIT_TYPE_FILE_PTR\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_TYPE_SIZE_T_const = (int)GCC_JIT_TYPE_SIZE_T };
     int64_t v = (GCC_JIT_TYPE_SIZE_T);
     printf("  | \"GCC_JIT_TYPE_SIZE_T\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_TYPE_CONST_CHAR_PTR_const = (int)GCC_JIT_TYPE_CONST_CHAR_PTR };
     int64_t v = (GCC_JIT_TYPE_CONST_CHAR_PTR);
     printf("  | \"GCC_JIT_TYPE_CONST_CHAR_PTR\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_TYPE_LONG_DOUBLE_const = (int)GCC_JIT_TYPE_LONG_DOUBLE };
     int64_t v = (GCC_JIT_TYPE_LONG_DOUBLE);
     printf("  | \"GCC_JIT_TYPE_LONG_DOUBLE\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_TYPE_DOUBLE_const = (int)GCC_JIT_TYPE_DOUBLE };
     int64_t v = (GCC_JIT_TYPE_DOUBLE);
     printf("  | \"GCC_JIT_TYPE_DOUBLE\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_TYPE_FLOAT_const = (int)GCC_JIT_TYPE_FLOAT };
     int64_t v = (GCC_JIT_TYPE_FLOAT);
     printf("  | \"GCC_JIT_TYPE_FLOAT\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_TYPE_UNSIGNED_LONG_LONG_const = (int)GCC_JIT_TYPE_UNSIGNED_LONG_LONG };
     int64_t v = (GCC_JIT_TYPE_UNSIGNED_LONG_LONG);
     printf("  | \"GCC_JIT_TYPE_UNSIGNED_LONG_LONG\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_TYPE_LONG_LONG_const = (int)GCC_JIT_TYPE_LONG_LONG };
     int64_t v = (GCC_JIT_TYPE_LONG_LONG);
     printf("  | \"GCC_JIT_TYPE_LONG_LONG\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_TYPE_UNSIGNED_LONG_const = (int)GCC_JIT_TYPE_UNSIGNED_LONG };
     int64_t v = (GCC_JIT_TYPE_UNSIGNED_LONG);
     printf("  | \"GCC_JIT_TYPE_UNSIGNED_LONG\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_TYPE_LONG_const = (int)GCC_JIT_TYPE_LONG };
     int64_t v = (GCC_JIT_TYPE_LONG);
     printf("  | \"GCC_JIT_TYPE_LONG\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_TYPE_UNSIGNED_INT_const = (int)GCC_JIT_TYPE_UNSIGNED_INT };
     int64_t v = (GCC_JIT_TYPE_UNSIGNED_INT);
     printf("  | \"GCC_JIT_TYPE_UNSIGNED_INT\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_TYPE_INT_const = (int)GCC_JIT_TYPE_INT };
     int64_t v = (GCC_JIT_TYPE_INT);
     printf("  | \"GCC_JIT_TYPE_INT\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_TYPE_UNSIGNED_SHORT_const = (int)GCC_JIT_TYPE_UNSIGNED_SHORT };
     int64_t v = (GCC_JIT_TYPE_UNSIGNED_SHORT);
     printf("  | \"GCC_JIT_TYPE_UNSIGNED_SHORT\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_TYPE_SHORT_const = (int)GCC_JIT_TYPE_SHORT };
     int64_t v = (GCC_JIT_TYPE_SHORT);
     printf("  | \"GCC_JIT_TYPE_SHORT\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_TYPE_UNSIGNED_CHAR_const = (int)GCC_JIT_TYPE_UNSIGNED_CHAR };
     int64_t v = (GCC_JIT_TYPE_UNSIGNED_CHAR);
     printf("  | \"GCC_JIT_TYPE_UNSIGNED_CHAR\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_TYPE_SIGNED_CHAR_const = (int)GCC_JIT_TYPE_SIGNED_CHAR };
     int64_t v = (GCC_JIT_TYPE_SIGNED_CHAR);
     printf("  | \"GCC_JIT_TYPE_SIGNED_CHAR\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_TYPE_CHAR_const = (int)GCC_JIT_TYPE_CHAR };
     int64_t v = (GCC_JIT_TYPE_CHAR);
     printf("  | \"GCC_JIT_TYPE_CHAR\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_TYPE_BOOL_const = (int)GCC_JIT_TYPE_BOOL };
     int64_t v = (GCC_JIT_TYPE_BOOL);
     printf("  | \"GCC_JIT_TYPE_BOOL\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_TYPE_VOID_PTR_const = (int)GCC_JIT_TYPE_VOID_PTR };
     int64_t v = (GCC_JIT_TYPE_VOID_PTR);
     printf("  | \"GCC_JIT_TYPE_VOID_PTR\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_TYPE_VOID_const = (int)GCC_JIT_TYPE_VOID };
     int64_t v = (GCC_JIT_TYPE_VOID);
     printf("  | \"GCC_JIT_TYPE_VOID\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_OUTPUT_KIND_EXECUTABLE_const = (int)GCC_JIT_OUTPUT_KIND_EXECUTABLE };
     int64_t v = (GCC_JIT_OUTPUT_KIND_EXECUTABLE);
     printf("  | \"GCC_JIT_OUTPUT_KIND_EXECUTABLE\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_OUTPUT_KIND_DYNAMIC_LIBRARY_const = (int)GCC_JIT_OUTPUT_KIND_DYNAMIC_LIBRARY };
     int64_t v = (GCC_JIT_OUTPUT_KIND_DYNAMIC_LIBRARY);
     printf("  | \"GCC_JIT_OUTPUT_KIND_DYNAMIC_LIBRARY\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_OUTPUT_KIND_OBJECT_FILE_const = (int)GCC_JIT_OUTPUT_KIND_OBJECT_FILE };
     int64_t v = (GCC_JIT_OUTPUT_KIND_OBJECT_FILE);
     printf("  | \"GCC_JIT_OUTPUT_KIND_OBJECT_FILE\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_OUTPUT_KIND_ASSEMBLER_const = (int)GCC_JIT_OUTPUT_KIND_ASSEMBLER };
     int64_t v = (GCC_JIT_OUTPUT_KIND_ASSEMBLER);
     printf("  | \"GCC_JIT_OUTPUT_KIND_ASSEMBLER\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_BOOL_OPTION_KEEP_INTERMEDIATES_const = (int)GCC_JIT_BOOL_OPTION_KEEP_INTERMEDIATES };
     int64_t v = (GCC_JIT_BOOL_OPTION_KEEP_INTERMEDIATES);
     printf("  | \"GCC_JIT_BOOL_OPTION_KEEP_INTERMEDIATES\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_BOOL_OPTION_SELFCHECK_GC_const = (int)GCC_JIT_BOOL_OPTION_SELFCHECK_GC };
     int64_t v = (GCC_JIT_BOOL_OPTION_SELFCHECK_GC);
     printf("  | \"GCC_JIT_BOOL_OPTION_SELFCHECK_GC\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_BOOL_OPTION_DUMP_EVERYTHING_const = (int)GCC_JIT_BOOL_OPTION_DUMP_EVERYTHING };
     int64_t v = (GCC_JIT_BOOL_OPTION_DUMP_EVERYTHING);
     printf("  | \"GCC_JIT_BOOL_OPTION_DUMP_EVERYTHING\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_BOOL_OPTION_DUMP_SUMMARY_const = (int)GCC_JIT_BOOL_OPTION_DUMP_SUMMARY };
     int64_t v = (GCC_JIT_BOOL_OPTION_DUMP_SUMMARY);
     printf("  | \"GCC_JIT_BOOL_OPTION_DUMP_SUMMARY\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_BOOL_OPTION_DUMP_GENERATED_CODE_const = (int)GCC_JIT_BOOL_OPTION_DUMP_GENERATED_CODE };
     int64_t v = (GCC_JIT_BOOL_OPTION_DUMP_GENERATED_CODE);
     printf("  | \"GCC_JIT_BOOL_OPTION_DUMP_GENERATED_CODE\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_BOOL_OPTION_DUMP_INITIAL_GIMPLE_const = (int)GCC_JIT_BOOL_OPTION_DUMP_INITIAL_GIMPLE };
     int64_t v = (GCC_JIT_BOOL_OPTION_DUMP_INITIAL_GIMPLE);
     printf("  | \"GCC_JIT_BOOL_OPTION_DUMP_INITIAL_GIMPLE\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_BOOL_OPTION_DUMP_INITIAL_TREE_const = (int)GCC_JIT_BOOL_OPTION_DUMP_INITIAL_TREE };
     int64_t v = (GCC_JIT_BOOL_OPTION_DUMP_INITIAL_TREE);
     printf("  | \"GCC_JIT_BOOL_OPTION_DUMP_INITIAL_TREE\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_BOOL_OPTION_DEBUGINFO_const = (int)GCC_JIT_BOOL_OPTION_DEBUGINFO };
     int64_t v = (GCC_JIT_BOOL_OPTION_DEBUGINFO);
     printf("  | \"GCC_JIT_BOOL_OPTION_DEBUGINFO\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL_const = (int)GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL };
     int64_t v = (GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL);
     printf("  | \"GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  {
     enum { check_GCC_JIT_STR_OPTION_PROGNAME_const = (int)GCC_JIT_STR_OPTION_PROGNAME };
     int64_t v = (GCC_JIT_STR_OPTION_PROGNAME);
     printf("  | \"GCC_JIT_STR_OPTION_PROGNAME\", Ctypes_static.Primitive Cstubs_internals.Int64_t ->\n    %lldL\n",
            v);
     
  }
  puts("  | s, _ -> failwith (\"unmatched constant: \"^ s)");
  puts("");
  puts("let enum (type a) name ?unexpected (alist : (a * int64) list) =");
  puts("  match name with");
  printf("  | \"gcc_jit_comparison\" -> \n    Cstubs_internals.build_enum_type \"gcc_jit_comparison\" Ctypes_static.%s ?unexpected alist\n",
         ctypes_arithmetic_type_name(CTYPES_CLASSIFY_ARITHMETIC_TYPE(enum gcc_jit_comparison)));
  printf("  | \"gcc_jit_binary_op\" -> \n    Cstubs_internals.build_enum_type \"gcc_jit_binary_op\" Ctypes_static.%s ?unexpected alist\n",
         ctypes_arithmetic_type_name(CTYPES_CLASSIFY_ARITHMETIC_TYPE(enum gcc_jit_binary_op)));
  printf("  | \"gcc_jit_unary_op\" -> \n    Cstubs_internals.build_enum_type \"gcc_jit_unary_op\" Ctypes_static.%s ?unexpected alist\n",
         ctypes_arithmetic_type_name(CTYPES_CLASSIFY_ARITHMETIC_TYPE(enum gcc_jit_unary_op)));
  printf("  | \"gcc_jit_global_kind\" -> \n    Cstubs_internals.build_enum_type \"gcc_jit_global_kind\" Ctypes_static.%s ?unexpected alist\n",
         ctypes_arithmetic_type_name(CTYPES_CLASSIFY_ARITHMETIC_TYPE(enum gcc_jit_global_kind)));
  printf("  | \"gcc_jit_function_kind\" -> \n    Cstubs_internals.build_enum_type \"gcc_jit_function_kind\" Ctypes_static.%s ?unexpected alist\n",
         ctypes_arithmetic_type_name(CTYPES_CLASSIFY_ARITHMETIC_TYPE(enum gcc_jit_function_kind)));
  printf("  | \"gcc_jit_types\" -> \n    Cstubs_internals.build_enum_type \"gcc_jit_types\" Ctypes_static.%s ?unexpected alist\n",
         ctypes_arithmetic_type_name(CTYPES_CLASSIFY_ARITHMETIC_TYPE(enum gcc_jit_types)));
  printf("  | \"gcc_jit_output_kind\" -> \n    Cstubs_internals.build_enum_type \"gcc_jit_output_kind\" Ctypes_static.%s ?unexpected alist\n",
         ctypes_arithmetic_type_name(CTYPES_CLASSIFY_ARITHMETIC_TYPE(enum gcc_jit_output_kind)));
  printf("  | \"gcc_jit_bool_option\" -> \n    Cstubs_internals.build_enum_type \"gcc_jit_bool_option\" Ctypes_static.%s ?unexpected alist\n",
         ctypes_arithmetic_type_name(CTYPES_CLASSIFY_ARITHMETIC_TYPE(enum gcc_jit_bool_option)));
  printf("  | \"gcc_jit_int_option\" -> \n    Cstubs_internals.build_enum_type \"gcc_jit_int_option\" Ctypes_static.%s ?unexpected alist\n",
         ctypes_arithmetic_type_name(CTYPES_CLASSIFY_ARITHMETIC_TYPE(enum gcc_jit_int_option)));
  printf("  | \"gcc_jit_str_option\" -> \n    Cstubs_internals.build_enum_type \"gcc_jit_str_option\" Ctypes_static.%s ?unexpected alist\n",
         ctypes_arithmetic_type_name(CTYPES_CLASSIFY_ARITHMETIC_TYPE(enum gcc_jit_str_option)));
  puts("  | s ->");
  puts("    failwith (\"unmatched enum: \"^ s)");
  
  return 0;
}
