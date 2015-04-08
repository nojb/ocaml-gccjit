module CI = Cstubs_internals

external gccjit_stub__1_gcc_jit_context_acquire : unit -> CI.voidp
  = "gccjit_stub__1_gcc_jit_context_acquire" 

external gccjit_stub__2_gcc_jit_context_release : _ CI.fatptr -> unit
  = "gccjit_stub__2_gcc_jit_context_release" 

external gccjit_stub__3_gcc_jit_context_set_str_option
  : _ CI.fatptr -> Unsigned.uint32 -> _ CI.fatptr -> unit
  = "gccjit_stub__3_gcc_jit_context_set_str_option" 

external gccjit_stub__4_gcc_jit_context_set_int_option
  : _ CI.fatptr -> Unsigned.uint32 -> int -> unit
  = "gccjit_stub__4_gcc_jit_context_set_int_option" 

external gccjit_stub__5_gcc_jit_context_set_bool_option
  : _ CI.fatptr -> Unsigned.uint32 -> bool -> unit
  = "gccjit_stub__5_gcc_jit_context_set_bool_option" 

external gccjit_stub__6_gcc_jit_context_compile : _ CI.fatptr -> CI.voidp
  = "gccjit_stub__6_gcc_jit_context_compile" 

external gccjit_stub__7_gcc_jit_context_compile_to_file
  : _ CI.fatptr -> Unsigned.uint32 -> _ CI.fatptr -> unit
  = "gccjit_stub__7_gcc_jit_context_compile_to_file" 

external gccjit_stub__8_gcc_jit_context_dump_to_file
  : _ CI.fatptr -> _ CI.fatptr -> int -> unit
  = "gccjit_stub__8_gcc_jit_context_dump_to_file" 

external gccjit_stub__9_gcc_jit_context_set_logfile
  : _ CI.fatptr -> _ CI.fatptr -> int -> int -> unit
  = "gccjit_stub__9_gcc_jit_context_set_logfile" 

external gccjit_stub__10_gcc_jit_context_get_first_error
  : _ CI.fatptr -> CI.voidp
  = "gccjit_stub__10_gcc_jit_context_get_first_error" 

external gccjit_stub__11_gcc_jit_context_get_last_error
  : _ CI.fatptr -> CI.voidp
  = "gccjit_stub__11_gcc_jit_context_get_last_error" 

external gccjit_stub__12_gcc_jit_result_get_code
  : _ CI.fatptr -> _ CI.fatptr -> CI.voidp
  = "gccjit_stub__12_gcc_jit_result_get_code" 

external gccjit_stub__13_gcc_jit_get_global
  : _ CI.fatptr -> _ CI.fatptr -> CI.voidp
  = "gccjit_stub__13_gcc_jit_get_global" 

external gccjit_stub__14_gcc_jit_result_release : _ CI.fatptr -> unit
  = "gccjit_stub__14_gcc_jit_result_release" 

external gccjit_stub__15_gcc_jit_object_get_context : _ CI.fatptr -> CI.voidp
  = "gccjit_stub__15_gcc_jit_object_get_context" 

external gccjit_stub__16_gcc_jit_object_get_debug_string
  : _ CI.fatptr -> CI.voidp
  = "gccjit_stub__16_gcc_jit_object_get_debug_string" 

external gccjit_stub__17_gcc_jit_context_new_location
  : _ CI.fatptr -> _ CI.fatptr -> int -> int -> CI.voidp
  = "gccjit_stub__17_gcc_jit_context_new_location" 

external gccjit_stub__18_gcc_jit_location_as_object : _ CI.fatptr -> CI.voidp
  = "gccjit_stub__18_gcc_jit_location_as_object" 

external gccjit_stub__19_gcc_jit_type_as_object : _ CI.fatptr -> CI.voidp
  = "gccjit_stub__19_gcc_jit_type_as_object" 

external gccjit_stub__20_gcc_jit_context_get_type
  : _ CI.fatptr -> Unsigned.uint32 -> CI.voidp
  = "gccjit_stub__20_gcc_jit_context_get_type" 

external gccjit_stub__21_gcc_jit_context_get_int_type
  : _ CI.fatptr -> int -> int -> CI.voidp
  = "gccjit_stub__21_gcc_jit_context_get_int_type" 

external gccjit_stub__22_gcc_jit_type_get_pointer : _ CI.fatptr -> CI.voidp
  = "gccjit_stub__22_gcc_jit_type_get_pointer" 

external gccjit_stub__23_gcc_jit_type_get_const : _ CI.fatptr -> CI.voidp
  = "gccjit_stub__23_gcc_jit_type_get_const" 

external gccjit_stub__24_gcc_jit_type_get_volatile : _ CI.fatptr -> CI.voidp
  = "gccjit_stub__24_gcc_jit_type_get_volatile" 

external gccjit_stub__25_gcc_jit_context_new_array_type
  : _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> int -> CI.voidp
  = "gccjit_stub__25_gcc_jit_context_new_array_type" 

external gccjit_stub__26_gcc_jit_context_new_field
  : _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> CI.voidp
  = "gccjit_stub__26_gcc_jit_context_new_field" 

external gccjit_stub__27_gcc_jit_field_as_object : _ CI.fatptr -> CI.voidp
  = "gccjit_stub__27_gcc_jit_field_as_object" 

external gccjit_stub__28_gcc_jit_context_new_struct_type
  : _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> int -> _ CI.fatptr ->
    CI.voidp = "gccjit_stub__28_gcc_jit_context_new_struct_type" 

external gccjit_stub__29_gcc_jit_context_new_opaque_struct
  : _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> CI.voidp
  = "gccjit_stub__29_gcc_jit_context_new_opaque_struct" 

external gccjit_stub__30_gcc_jit_struct_as_type : _ CI.fatptr -> CI.voidp
  = "gccjit_stub__30_gcc_jit_struct_as_type" 

external gccjit_stub__31_gcc_jit_struct_set_fields
  : _ CI.fatptr -> _ CI.fatptr -> int -> _ CI.fatptr -> unit
  = "gccjit_stub__31_gcc_jit_struct_set_fields" 

external gccjit_stub__32_gcc_jit_context_new_union_type
  : _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> int -> _ CI.fatptr ->
    CI.voidp = "gccjit_stub__32_gcc_jit_context_new_union_type" 

external gccjit_stub__33_gcc_jit_context_new_function_ptr_type
  : _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> int -> _ CI.fatptr -> 
    int -> CI.voidp
  =
  "gccjit_stub__33_gcc_jit_context_new_function_ptr_type_byte6" "gccjit_stub__33_gcc_jit_context_new_function_ptr_type"
  

external gccjit_stub__34_gcc_jit_context_new_param
  : _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> CI.voidp
  = "gccjit_stub__34_gcc_jit_context_new_param" 

external gccjit_stub__35_gcc_jit_param_as_object : _ CI.fatptr -> CI.voidp
  = "gccjit_stub__35_gcc_jit_param_as_object" 

external gccjit_stub__36_gcc_jit_param_as_lvalue : _ CI.fatptr -> CI.voidp
  = "gccjit_stub__36_gcc_jit_param_as_lvalue" 

external gccjit_stub__37_gcc_jit_param_as_rvalue : _ CI.fatptr -> CI.voidp
  = "gccjit_stub__37_gcc_jit_param_as_rvalue" 

external gccjit_stub__38_gcc_jit_context_new_function
  : _ CI.fatptr -> _ CI.fatptr -> Unsigned.uint32 -> _ CI.fatptr ->
    _ CI.fatptr -> int -> _ CI.fatptr -> int -> CI.voidp
  =
  "gccjit_stub__38_gcc_jit_context_new_function_byte8" "gccjit_stub__38_gcc_jit_context_new_function"
  

external gccjit_stub__39_gcc_jit_context_get_builtin_function
  : _ CI.fatptr -> _ CI.fatptr -> CI.voidp
  = "gccjit_stub__39_gcc_jit_context_get_builtin_function" 

external gccjit_stub__40_gcc_jit_function_as_object : _ CI.fatptr -> CI.voidp
  = "gccjit_stub__40_gcc_jit_function_as_object" 

external gccjit_stub__41_gcc_jit_function_get_param
  : _ CI.fatptr -> int -> CI.voidp
  = "gccjit_stub__41_gcc_jit_function_get_param" 

external gccjit_stub__42_gcc_jit_function_dump_to_dot
  : _ CI.fatptr -> _ CI.fatptr -> unit
  = "gccjit_stub__42_gcc_jit_function_dump_to_dot" 

external gccjit_stub__43_gcc_jit_functin_new_block
  : _ CI.fatptr -> _ CI.fatptr -> CI.voidp
  = "gccjit_stub__43_gcc_jit_functin_new_block" 

external gccjit_stub__44_gcc_jit_block_as_object : _ CI.fatptr -> CI.voidp
  = "gccjit_stub__44_gcc_jit_block_as_object" 

external gccjit_stub__45_gcc_jit_block_get_function : _ CI.fatptr -> CI.voidp
  = "gccjit_stub__45_gcc_jit_block_get_function" 

external gccjit_stub__46_gcc_jit_context_new_global
  : _ CI.fatptr -> _ CI.fatptr -> Unsigned.uint32 -> _ CI.fatptr ->
    _ CI.fatptr -> CI.voidp = "gccjit_stub__46_gcc_jit_context_new_global" 

external gccjit_stub__47_gcc_jit_lvalue_as_object : _ CI.fatptr -> CI.voidp
  = "gccjit_stub__47_gcc_jit_lvalue_as_object" 

external gccjit_stub__48_gcc_jit_lvalue_as_rvalue : _ CI.fatptr -> CI.voidp
  = "gccjit_stub__48_gcc_jit_lvalue_as_rvalue" 

external gccjit_stub__49_gcc_jit_rvalue_as_object : _ CI.fatptr -> CI.voidp
  = "gccjit_stub__49_gcc_jit_rvalue_as_object" 

external gccjit_stub__50_gcc_jit_rvalue_get_type : _ CI.fatptr -> CI.voidp
  = "gccjit_stub__50_gcc_jit_rvalue_get_type" 

external gccjit_stub__51_gcc_jit_context_new_rvalue_from_int
  : _ CI.fatptr -> _ CI.fatptr -> int -> CI.voidp
  = "gccjit_stub__51_gcc_jit_context_new_rvalue_from_int" 

external gccjit_stub__52_gcc_jit_context_new_rvalue_from_long
  : _ CI.fatptr -> _ CI.fatptr -> int -> CI.voidp
  = "gccjit_stub__52_gcc_jit_context_new_rvalue_from_long" 

external gccjit_stub__53_gcc_jit_context_zero
  : _ CI.fatptr -> _ CI.fatptr -> CI.voidp
  = "gccjit_stub__53_gcc_jit_context_zero" 

external gccjit_stub__54_gcc_jit_context_one
  : _ CI.fatptr -> _ CI.fatptr -> CI.voidp
  = "gccjit_stub__54_gcc_jit_context_one" 

external gccjit_stub__55_gcc_jit_context_new_rvalue_from_double
  : _ CI.fatptr -> _ CI.fatptr -> float -> CI.voidp
  = "gccjit_stub__55_gcc_jit_context_new_rvalue_from_double" 

external gccjit_stub__56_gcc_jit_context_new_rvalue_from_ptr
  : _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> CI.voidp
  = "gccjit_stub__56_gcc_jit_context_new_rvalue_from_ptr" 

external gccjit_stub__57_gcc_jit_context_null
  : _ CI.fatptr -> _ CI.fatptr -> CI.voidp
  = "gccjit_stub__57_gcc_jit_context_null" 

external gccjit_stub__58_gcc_jit_context_new_string_literal
  : _ CI.fatptr -> _ CI.fatptr -> CI.voidp
  = "gccjit_stub__58_gcc_jit_context_new_string_literal" 

external gccjit_stub__59_gcc_jit_context_new_unary_op
  : _ CI.fatptr -> _ CI.fatptr -> Unsigned.uint32 -> _ CI.fatptr ->
    _ CI.fatptr -> CI.voidp = "gccjit_stub__59_gcc_jit_context_new_unary_op" 

external gccjit_stub__60_gcc_jit_context_new_binary_op
  : _ CI.fatptr -> _ CI.fatptr -> Unsigned.uint32 -> _ CI.fatptr ->
    _ CI.fatptr -> _ CI.fatptr -> CI.voidp
  =
  "gccjit_stub__60_gcc_jit_context_new_binary_op_byte6" "gccjit_stub__60_gcc_jit_context_new_binary_op"
  

external gccjit_stub__61_gcc_jit_context_new_comparison
  : _ CI.fatptr -> _ CI.fatptr -> Unsigned.uint32 -> _ CI.fatptr ->
    _ CI.fatptr -> CI.voidp
  = "gccjit_stub__61_gcc_jit_context_new_comparison" 

external gccjit_stub__62_gcc_jit_context_new_call
  : _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> int -> _ CI.fatptr ->
    CI.voidp = "gccjit_stub__62_gcc_jit_context_new_call" 

external gccjit_stub__63_gcc_jit_context_new_call_through_ptr
  : _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> int -> _ CI.fatptr ->
    CI.voidp = "gccjit_stub__63_gcc_jit_context_new_call_through_ptr" 

external gccjit_stub__64_gcc_jit_context_new_cast
  : _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> CI.voidp
  = "gccjit_stub__64_gcc_jit_context_new_cast" 

external gccjit_stub__65_gcc_jit_context_new_array_access
  : _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> CI.voidp
  = "gccjit_stub__65_gcc_jit_context_new_array_access" 

external gccjit_stub__66_gcc_jit_lvalue_access_field
  : _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> CI.voidp
  = "gccjit_stub__66_gcc_jit_lvalue_access_field" 

external gccjit_stub__67_gcc_jit_rvalue_access_field
  : _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> CI.voidp
  = "gccjit_stub__67_gcc_jit_rvalue_access_field" 

external gccjit_stub__68_gcc_jit_rvalue_dereference_field
  : _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> CI.voidp
  = "gccjit_stub__68_gcc_jit_rvalue_dereference_field" 

external gccjit_stub__69_gcc_jit_rvalue_dereference
  : _ CI.fatptr -> _ CI.fatptr -> CI.voidp
  = "gccjit_stub__69_gcc_jit_rvalue_dereference" 

external gccjit_stub__70_gcc_jit_lvalue_get_address
  : _ CI.fatptr -> _ CI.fatptr -> CI.voidp
  = "gccjit_stub__70_gcc_jit_lvalue_get_address" 

external gccjit_stub__71_gcc_jit_function_new_local
  : _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> CI.voidp
  = "gccjit_stub__71_gcc_jit_function_new_local" 

external gccjit_stub__72_gcc_jit_block_add_eval
  : _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> unit
  = "gccjit_stub__72_gcc_jit_block_add_eval" 

external gccjit_stub__73_gcc_jit_block_add_assignment
  : _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> unit
  = "gccjit_stub__73_gcc_jit_block_add_assignment" 

external gccjit_stub__74_gcc_jit_block_add_assignment_op
  : _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> Unsigned.uint32 ->
    _ CI.fatptr -> unit = "gccjit_stub__74_gcc_jit_block_add_assignment_op" 

external gccjit_stub__75_gcc_jit_block_add_comment
  : _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> unit
  = "gccjit_stub__75_gcc_jit_block_add_comment" 

external gccjit_stub__76_gcc_jit_block_end_with_conditional
  : _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr ->
    _ CI.fatptr -> unit
  = "gccjit_stub__76_gcc_jit_block_end_with_conditional" 

external gccjit_stub__77_gcc_jit_block_end_with_jump
  : _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> unit
  = "gccjit_stub__77_gcc_jit_block_end_with_jump" 

external gccjit_stub__78_gcc_jit_block_end_with_return
  : _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> unit
  = "gccjit_stub__78_gcc_jit_block_end_with_return" 

external gccjit_stub__79_gcc_jit_block_end_with_void_return
  : _ CI.fatptr -> _ CI.fatptr -> unit
  = "gccjit_stub__79_gcc_jit_block_end_with_void_return" 

external gccjit_stub__80_gcc_jit_context_new_child_context
  : _ CI.fatptr -> CI.voidp
  = "gccjit_stub__80_gcc_jit_context_new_child_context" 

external gccjit_stub__81_gcc_jit_context_dump_reproducer_to_file
  : _ CI.fatptr -> _ CI.fatptr -> unit
  = "gccjit_stub__81_gcc_jit_context_dump_reproducer_to_file" 

type 'a fn = 'a

let foreign : type a b. string -> (a -> b) Ctypes.fn -> (a -> b) =
  fun name t -> match name, t with
| "gcc_jit_context_dump_reproducer_to_file",
  CI.Function
    (CI.Pointer x2,
     CI.Function
       (CI.View {CI.ty = CI.Pointer x5; write = x4; }, CI.Returns CI.Void)) ->
  (fun x1 x3 ->
    gccjit_stub__81_gcc_jit_context_dump_reproducer_to_file (CI.cptr x1)
    (CI.cptr (x4 x3)))
| "gcc_jit_context_new_child_context",
  CI.Function (CI.Pointer x7, CI.Returns (CI.Pointer x8)) ->
  (fun x6 ->
    CI.make_ptr x8
      (gccjit_stub__80_gcc_jit_context_new_child_context (CI.cptr x6)))
| "gcc_jit_block_end_with_void_return",
  CI.Function
    (CI.Pointer x10, CI.Function (CI.Pointer x12, CI.Returns CI.Void)) ->
  (fun x9 x11 ->
    gccjit_stub__79_gcc_jit_block_end_with_void_return (CI.cptr x9)
    (CI.cptr x11))
| "gcc_jit_block_end_with_return",
  CI.Function
    (CI.Pointer x14,
     CI.Function
       (CI.Pointer x16, CI.Function (CI.Pointer x18, CI.Returns CI.Void))) ->
  (fun x13 x15 x17 ->
    gccjit_stub__78_gcc_jit_block_end_with_return (CI.cptr x13) (CI.cptr x15)
    (CI.cptr x17))
| "gcc_jit_block_end_with_jump",
  CI.Function
    (CI.Pointer x20,
     CI.Function
       (CI.Pointer x22, CI.Function (CI.Pointer x24, CI.Returns CI.Void))) ->
  (fun x19 x21 x23 ->
    gccjit_stub__77_gcc_jit_block_end_with_jump (CI.cptr x19) (CI.cptr x21)
    (CI.cptr x23))
| "gcc_jit_block_end_with_conditional",
  CI.Function
    (CI.Pointer x26,
     CI.Function
       (CI.Pointer x28,
        CI.Function
          (CI.Pointer x30,
           CI.Function
             (CI.Pointer x32,
              CI.Function (CI.Pointer x34, CI.Returns CI.Void))))) ->
  (fun x25 x27 x29 x31 x33 ->
    gccjit_stub__76_gcc_jit_block_end_with_conditional (CI.cptr x25)
    (CI.cptr x27) (CI.cptr x29) (CI.cptr x31) (CI.cptr x33))
| "gcc_jit_block_add_comment",
  CI.Function
    (CI.Pointer x36,
     CI.Function
       (CI.Pointer x38,
        CI.Function
          (CI.View {CI.ty = CI.Pointer x41; write = x40; },
           CI.Returns CI.Void))) ->
  (fun x35 x37 x39 ->
    gccjit_stub__75_gcc_jit_block_add_comment (CI.cptr x35) (CI.cptr x37)
    (CI.cptr (x40 x39)))
| "gcc_jit_block_add_assignment_op",
  CI.Function
    (CI.Pointer x43,
     CI.Function
       (CI.Pointer x45,
        CI.Function
          (CI.Pointer x47,
           CI.Function
             (CI.View {CI.ty = CI.Primitive CI.Uint32_t; write = x49; },
              CI.Function (CI.Pointer x51, CI.Returns CI.Void))))) ->
  (fun x42 x44 x46 x48 x50 ->
    gccjit_stub__74_gcc_jit_block_add_assignment_op (CI.cptr x42)
    (CI.cptr x44) (CI.cptr x46) (x49 x48) (CI.cptr x50))
| "gcc_jit_block_add_assignment",
  CI.Function
    (CI.Pointer x53,
     CI.Function
       (CI.Pointer x55,
        CI.Function
          (CI.Pointer x57, CI.Function (CI.Pointer x59, CI.Returns CI.Void)))) ->
  (fun x52 x54 x56 x58 ->
    gccjit_stub__73_gcc_jit_block_add_assignment (CI.cptr x52) (CI.cptr x54)
    (CI.cptr x56) (CI.cptr x58))
| "gcc_jit_block_add_eval",
  CI.Function
    (CI.Pointer x61,
     CI.Function
       (CI.Pointer x63, CI.Function (CI.Pointer x65, CI.Returns CI.Void))) ->
  (fun x60 x62 x64 ->
    gccjit_stub__72_gcc_jit_block_add_eval (CI.cptr x60) (CI.cptr x62)
    (CI.cptr x64))
| "gcc_jit_function_new_local",
  CI.Function
    (CI.Pointer x67,
     CI.Function
       (CI.Pointer x69,
        CI.Function
          (CI.Pointer x71,
           CI.Function
             (CI.View {CI.ty = CI.Pointer x74; write = x73; },
              CI.Returns (CI.Pointer x75))))) ->
  (fun x66 x68 x70 x72 ->
    CI.make_ptr x75
      (gccjit_stub__71_gcc_jit_function_new_local (CI.cptr x66) (CI.cptr x68)
       (CI.cptr x70) (CI.cptr (x73 x72))))
| "gcc_jit_lvalue_get_address",
  CI.Function
    (CI.Pointer x77,
     CI.Function (CI.Pointer x79, CI.Returns (CI.Pointer x80))) ->
  (fun x76 x78 ->
    CI.make_ptr x80
      (gccjit_stub__70_gcc_jit_lvalue_get_address (CI.cptr x76)
        (CI.cptr x78)))
| "gcc_jit_rvalue_dereference",
  CI.Function
    (CI.Pointer x82,
     CI.Function (CI.Pointer x84, CI.Returns (CI.Pointer x85))) ->
  (fun x81 x83 ->
    CI.make_ptr x85
      (gccjit_stub__69_gcc_jit_rvalue_dereference (CI.cptr x81)
        (CI.cptr x83)))
| "gcc_jit_rvalue_dereference_field",
  CI.Function
    (CI.Pointer x87,
     CI.Function
       (CI.Pointer x89,
        CI.Function (CI.Pointer x91, CI.Returns (CI.Pointer x92)))) ->
  (fun x86 x88 x90 ->
    CI.make_ptr x92
      (gccjit_stub__68_gcc_jit_rvalue_dereference_field (CI.cptr x86)
       (CI.cptr x88) (CI.cptr x90)))
| "gcc_jit_rvalue_access_field",
  CI.Function
    (CI.Pointer x94,
     CI.Function
       (CI.Pointer x96,
        CI.Function (CI.Pointer x98, CI.Returns (CI.Pointer x99)))) ->
  (fun x93 x95 x97 ->
    CI.make_ptr x99
      (gccjit_stub__67_gcc_jit_rvalue_access_field (CI.cptr x93)
       (CI.cptr x95) (CI.cptr x97)))
| "gcc_jit_lvalue_access_field",
  CI.Function
    (CI.Pointer x101,
     CI.Function
       (CI.Pointer x103,
        CI.Function (CI.Pointer x105, CI.Returns (CI.Pointer x106)))) ->
  (fun x100 x102 x104 ->
    CI.make_ptr x106
      (gccjit_stub__66_gcc_jit_lvalue_access_field (CI.cptr x100)
       (CI.cptr x102) (CI.cptr x104)))
| "gcc_jit_context_new_array_access",
  CI.Function
    (CI.Pointer x108,
     CI.Function
       (CI.Pointer x110,
        CI.Function
          (CI.Pointer x112,
           CI.Function (CI.Pointer x114, CI.Returns (CI.Pointer x115))))) ->
  (fun x107 x109 x111 x113 ->
    CI.make_ptr x115
      (gccjit_stub__65_gcc_jit_context_new_array_access (CI.cptr x107)
       (CI.cptr x109) (CI.cptr x111) (CI.cptr x113)))
| "gcc_jit_context_new_cast",
  CI.Function
    (CI.Pointer x117,
     CI.Function
       (CI.Pointer x119,
        CI.Function
          (CI.Pointer x121,
           CI.Function (CI.Pointer x123, CI.Returns (CI.Pointer x124))))) ->
  (fun x116 x118 x120 x122 ->
    CI.make_ptr x124
      (gccjit_stub__64_gcc_jit_context_new_cast (CI.cptr x116) (CI.cptr x118)
       (CI.cptr x120) (CI.cptr x122)))
| "gcc_jit_context_new_call_through_ptr",
  CI.Function
    (CI.Pointer x126,
     CI.Function
       (CI.Pointer x128,
        CI.Function
          (CI.Pointer x130,
           CI.Function
             (CI.Primitive CI.Int,
              CI.Function (CI.Pointer x133, CI.Returns (CI.Pointer x134)))))) ->
  (fun x125 x127 x129 x131 x132 ->
    CI.make_ptr x134
      (gccjit_stub__63_gcc_jit_context_new_call_through_ptr (CI.cptr x125)
       (CI.cptr x127) (CI.cptr x129) x131 (CI.cptr x132)))
| "gcc_jit_context_new_call",
  CI.Function
    (CI.Pointer x136,
     CI.Function
       (CI.Pointer x138,
        CI.Function
          (CI.Pointer x140,
           CI.Function
             (CI.Primitive CI.Int,
              CI.Function (CI.Pointer x143, CI.Returns (CI.Pointer x144)))))) ->
  (fun x135 x137 x139 x141 x142 ->
    CI.make_ptr x144
      (gccjit_stub__62_gcc_jit_context_new_call (CI.cptr x135) (CI.cptr x137)
       (CI.cptr x139) x141 (CI.cptr x142)))
| "gcc_jit_context_new_comparison",
  CI.Function
    (CI.Pointer x146,
     CI.Function
       (CI.Pointer x148,
        CI.Function
          (CI.View {CI.ty = CI.Primitive CI.Uint32_t; write = x150; },
           CI.Function
             (CI.Pointer x152,
              CI.Function (CI.Pointer x154, CI.Returns (CI.Pointer x155)))))) ->
  (fun x145 x147 x149 x151 x153 ->
    CI.make_ptr x155
      (gccjit_stub__61_gcc_jit_context_new_comparison (CI.cptr x145)
       (CI.cptr x147) (x150 x149) (CI.cptr x151) (CI.cptr x153)))
| "gcc_jit_context_new_binary_op",
  CI.Function
    (CI.Pointer x157,
     CI.Function
       (CI.Pointer x159,
        CI.Function
          (CI.View {CI.ty = CI.Primitive CI.Uint32_t; write = x161; },
           CI.Function
             (CI.Pointer x163,
              CI.Function
                (CI.Pointer x165,
                 CI.Function (CI.Pointer x167, CI.Returns (CI.Pointer x168))))))) ->
  (fun x156 x158 x160 x162 x164 x166 ->
    CI.make_ptr x168
      (gccjit_stub__60_gcc_jit_context_new_binary_op (CI.cptr x156)
       (CI.cptr x158) (x161 x160) (CI.cptr x162) (CI.cptr x164)
        (CI.cptr x166)))
| "gcc_jit_context_new_unary_op",
  CI.Function
    (CI.Pointer x170,
     CI.Function
       (CI.Pointer x172,
        CI.Function
          (CI.View {CI.ty = CI.Primitive CI.Uint32_t; write = x174; },
           CI.Function
             (CI.Pointer x176,
              CI.Function (CI.Pointer x178, CI.Returns (CI.Pointer x179)))))) ->
  (fun x169 x171 x173 x175 x177 ->
    CI.make_ptr x179
      (gccjit_stub__59_gcc_jit_context_new_unary_op (CI.cptr x169)
       (CI.cptr x171) (x174 x173) (CI.cptr x175) (CI.cptr x177)))
| "gcc_jit_context_new_string_literal",
  CI.Function
    (CI.Pointer x181,
     CI.Function
       (CI.View {CI.ty = CI.Pointer x184; write = x183; },
        CI.Returns (CI.Pointer x185))) ->
  (fun x180 x182 ->
    CI.make_ptr x185
      (gccjit_stub__58_gcc_jit_context_new_string_literal (CI.cptr x180)
        (CI.cptr (x183 x182))))
| "gcc_jit_context_null",
  CI.Function
    (CI.Pointer x187,
     CI.Function (CI.Pointer x189, CI.Returns (CI.Pointer x190))) ->
  (fun x186 x188 ->
    CI.make_ptr x190
      (gccjit_stub__57_gcc_jit_context_null (CI.cptr x186) (CI.cptr x188)))
| "gcc_jit_context_new_rvalue_from_ptr",
  CI.Function
    (CI.Pointer x192,
     CI.Function
       (CI.Pointer x194,
        CI.Function (CI.Pointer x196, CI.Returns (CI.Pointer x197)))) ->
  (fun x191 x193 x195 ->
    CI.make_ptr x197
      (gccjit_stub__56_gcc_jit_context_new_rvalue_from_ptr (CI.cptr x191)
       (CI.cptr x193) (CI.cptr x195)))
| "gcc_jit_context_new_rvalue_from_double",
  CI.Function
    (CI.Pointer x199,
     CI.Function
       (CI.Pointer x201,
        CI.Function (CI.Primitive CI.Float, CI.Returns (CI.Pointer x203)))) ->
  (fun x198 x200 x202 ->
    CI.make_ptr x203
      (gccjit_stub__55_gcc_jit_context_new_rvalue_from_double (CI.cptr x198)
       (CI.cptr x200) x202))
| "gcc_jit_context_one",
  CI.Function
    (CI.Pointer x205,
     CI.Function (CI.Pointer x207, CI.Returns (CI.Pointer x208))) ->
  (fun x204 x206 ->
    CI.make_ptr x208
      (gccjit_stub__54_gcc_jit_context_one (CI.cptr x204) (CI.cptr x206)))
| "gcc_jit_context_zero",
  CI.Function
    (CI.Pointer x210,
     CI.Function (CI.Pointer x212, CI.Returns (CI.Pointer x213))) ->
  (fun x209 x211 ->
    CI.make_ptr x213
      (gccjit_stub__53_gcc_jit_context_zero (CI.cptr x209) (CI.cptr x211)))
| "gcc_jit_context_new_rvalue_from_long",
  CI.Function
    (CI.Pointer x215,
     CI.Function
       (CI.Pointer x217,
        CI.Function (CI.Primitive CI.Int, CI.Returns (CI.Pointer x219)))) ->
  (fun x214 x216 x218 ->
    CI.make_ptr x219
      (gccjit_stub__52_gcc_jit_context_new_rvalue_from_long (CI.cptr x214)
       (CI.cptr x216) x218))
| "gcc_jit_context_new_rvalue_from_int",
  CI.Function
    (CI.Pointer x221,
     CI.Function
       (CI.Pointer x223,
        CI.Function (CI.Primitive CI.Int, CI.Returns (CI.Pointer x225)))) ->
  (fun x220 x222 x224 ->
    CI.make_ptr x225
      (gccjit_stub__51_gcc_jit_context_new_rvalue_from_int (CI.cptr x220)
       (CI.cptr x222) x224))
| "gcc_jit_rvalue_get_type",
  CI.Function (CI.Pointer x227, CI.Returns (CI.Pointer x228)) ->
  (fun x226 ->
    CI.make_ptr x228 (gccjit_stub__50_gcc_jit_rvalue_get_type (CI.cptr x226)))
| "gcc_jit_rvalue_as_object",
  CI.Function (CI.Pointer x230, CI.Returns (CI.Pointer x231)) ->
  (fun x229 ->
    CI.make_ptr x231
      (gccjit_stub__49_gcc_jit_rvalue_as_object (CI.cptr x229)))
| "gcc_jit_lvalue_as_rvalue",
  CI.Function (CI.Pointer x233, CI.Returns (CI.Pointer x234)) ->
  (fun x232 ->
    CI.make_ptr x234
      (gccjit_stub__48_gcc_jit_lvalue_as_rvalue (CI.cptr x232)))
| "gcc_jit_lvalue_as_object",
  CI.Function (CI.Pointer x236, CI.Returns (CI.Pointer x237)) ->
  (fun x235 ->
    CI.make_ptr x237
      (gccjit_stub__47_gcc_jit_lvalue_as_object (CI.cptr x235)))
| "gcc_jit_context_new_global",
  CI.Function
    (CI.Pointer x239,
     CI.Function
       (CI.Pointer x241,
        CI.Function
          (CI.View {CI.ty = CI.Primitive CI.Uint32_t; write = x243; },
           CI.Function
             (CI.Pointer x245,
              CI.Function
                (CI.View {CI.ty = CI.Pointer x248; write = x247; },
                 CI.Returns (CI.Pointer x249)))))) ->
  (fun x238 x240 x242 x244 x246 ->
    CI.make_ptr x249
      (gccjit_stub__46_gcc_jit_context_new_global (CI.cptr x238)
       (CI.cptr x240) (x243 x242) (CI.cptr x244) (CI.cptr (x247 x246))))
| "gcc_jit_block_get_function",
  CI.Function (CI.Pointer x251, CI.Returns (CI.Pointer x252)) ->
  (fun x250 ->
    CI.make_ptr x252
      (gccjit_stub__45_gcc_jit_block_get_function (CI.cptr x250)))
| "gcc_jit_block_as_object",
  CI.Function (CI.Pointer x254, CI.Returns (CI.Pointer x255)) ->
  (fun x253 ->
    CI.make_ptr x255 (gccjit_stub__44_gcc_jit_block_as_object (CI.cptr x253)))
| "gcc_jit_functin_new_block",
  CI.Function
    (CI.Pointer x257,
     CI.Function
       (CI.View {CI.ty = CI.Pointer x260; write = x259; },
        CI.Returns (CI.Pointer x261))) ->
  (fun x256 x258 ->
    CI.make_ptr x261
      (gccjit_stub__43_gcc_jit_functin_new_block (CI.cptr x256)
        (CI.cptr (x259 x258))))
| "gcc_jit_function_dump_to_dot",
  CI.Function
    (CI.Pointer x263,
     CI.Function
       (CI.View {CI.ty = CI.Pointer x266; write = x265; },
        CI.Returns CI.Void)) ->
  (fun x262 x264 ->
    gccjit_stub__42_gcc_jit_function_dump_to_dot (CI.cptr x262)
    (CI.cptr (x265 x264)))
| "gcc_jit_function_get_param",
  CI.Function
    (CI.Pointer x268,
     CI.Function (CI.Primitive CI.Int, CI.Returns (CI.Pointer x270))) ->
  (fun x267 x269 ->
    CI.make_ptr x270
      (gccjit_stub__41_gcc_jit_function_get_param (CI.cptr x267) x269))
| "gcc_jit_function_as_object",
  CI.Function (CI.Pointer x272, CI.Returns (CI.Pointer x273)) ->
  (fun x271 ->
    CI.make_ptr x273
      (gccjit_stub__40_gcc_jit_function_as_object (CI.cptr x271)))
| "gcc_jit_context_get_builtin_function",
  CI.Function
    (CI.Pointer x275,
     CI.Function
       (CI.View {CI.ty = CI.Pointer x278; write = x277; },
        CI.Returns (CI.Pointer x279))) ->
  (fun x274 x276 ->
    CI.make_ptr x279
      (gccjit_stub__39_gcc_jit_context_get_builtin_function (CI.cptr x274)
        (CI.cptr (x277 x276))))
| "gcc_jit_context_new_function",
  CI.Function
    (CI.Pointer x281,
     CI.Function
       (CI.Pointer x283,
        CI.Function
          (CI.View {CI.ty = CI.Primitive CI.Uint32_t; write = x285; },
           CI.Function
             (CI.Pointer x287,
              CI.Function
                (CI.View {CI.ty = CI.Pointer x290; write = x289; },
                 CI.Function
                   (CI.Primitive CI.Int,
                    CI.Function
                      (CI.Pointer x293,
                       CI.Function
                         (CI.Primitive CI.Int, CI.Returns (CI.Pointer x295))))))))) ->
  (fun x280 x282 x284 x286 x288 x291 x292 x294 ->
    CI.make_ptr x295
      (gccjit_stub__38_gcc_jit_context_new_function (CI.cptr x280)
       (CI.cptr x282) (x285 x284) (CI.cptr x286) (CI.cptr (x289 x288)) x291
       (CI.cptr x292) x294))
| "gcc_jit_param_as_rvalue",
  CI.Function (CI.Pointer x297, CI.Returns (CI.Pointer x298)) ->
  (fun x296 ->
    CI.make_ptr x298 (gccjit_stub__37_gcc_jit_param_as_rvalue (CI.cptr x296)))
| "gcc_jit_param_as_lvalue",
  CI.Function (CI.Pointer x300, CI.Returns (CI.Pointer x301)) ->
  (fun x299 ->
    CI.make_ptr x301 (gccjit_stub__36_gcc_jit_param_as_lvalue (CI.cptr x299)))
| "gcc_jit_param_as_object",
  CI.Function (CI.Pointer x303, CI.Returns (CI.Pointer x304)) ->
  (fun x302 ->
    CI.make_ptr x304 (gccjit_stub__35_gcc_jit_param_as_object (CI.cptr x302)))
| "gcc_jit_context_new_param",
  CI.Function
    (CI.Pointer x306,
     CI.Function
       (CI.Pointer x308,
        CI.Function
          (CI.Pointer x310,
           CI.Function
             (CI.View {CI.ty = CI.Pointer x313; write = x312; },
              CI.Returns (CI.Pointer x314))))) ->
  (fun x305 x307 x309 x311 ->
    CI.make_ptr x314
      (gccjit_stub__34_gcc_jit_context_new_param (CI.cptr x305)
       (CI.cptr x307) (CI.cptr x309) (CI.cptr (x312 x311))))
| "gcc_jit_context_new_function_ptr_type",
  CI.Function
    (CI.Pointer x316,
     CI.Function
       (CI.Pointer x318,
        CI.Function
          (CI.Pointer x320,
           CI.Function
             (CI.Primitive CI.Int,
              CI.Function
                (CI.Pointer x323,
                 CI.Function
                   (CI.Primitive CI.Int, CI.Returns (CI.Pointer x325))))))) ->
  (fun x315 x317 x319 x321 x322 x324 ->
    CI.make_ptr x325
      (gccjit_stub__33_gcc_jit_context_new_function_ptr_type (CI.cptr x315)
       (CI.cptr x317) (CI.cptr x319) x321 (CI.cptr x322) x324))
| "gcc_jit_context_new_union_type",
  CI.Function
    (CI.Pointer x327,
     CI.Function
       (CI.Pointer x329,
        CI.Function
          (CI.View {CI.ty = CI.Pointer x332; write = x331; },
           CI.Function
             (CI.Primitive CI.Int,
              CI.Function (CI.Pointer x335, CI.Returns (CI.Pointer x336)))))) ->
  (fun x326 x328 x330 x333 x334 ->
    CI.make_ptr x336
      (gccjit_stub__32_gcc_jit_context_new_union_type (CI.cptr x326)
       (CI.cptr x328) (CI.cptr (x331 x330)) x333 (CI.cptr x334)))
| "gcc_jit_struct_set_fields",
  CI.Function
    (CI.Pointer x338,
     CI.Function
       (CI.Pointer x340,
        CI.Function
          (CI.Primitive CI.Int,
           CI.Function (CI.Pointer x343, CI.Returns CI.Void)))) ->
  (fun x337 x339 x341 x342 ->
    gccjit_stub__31_gcc_jit_struct_set_fields (CI.cptr x337) (CI.cptr x339)
    x341 (CI.cptr x342))
| "gcc_jit_struct_as_type",
  CI.Function (CI.Pointer x345, CI.Returns (CI.Pointer x346)) ->
  (fun x344 ->
    CI.make_ptr x346 (gccjit_stub__30_gcc_jit_struct_as_type (CI.cptr x344)))
| "gcc_jit_context_new_opaque_struct",
  CI.Function
    (CI.Pointer x348,
     CI.Function
       (CI.Pointer x350,
        CI.Function
          (CI.View {CI.ty = CI.Pointer x353; write = x352; },
           CI.Returns (CI.Pointer x354)))) ->
  (fun x347 x349 x351 ->
    CI.make_ptr x354
      (gccjit_stub__29_gcc_jit_context_new_opaque_struct (CI.cptr x347)
       (CI.cptr x349) (CI.cptr (x352 x351))))
| "gcc_jit_context_new_struct_type",
  CI.Function
    (CI.Pointer x356,
     CI.Function
       (CI.Pointer x358,
        CI.Function
          (CI.View {CI.ty = CI.Pointer x361; write = x360; },
           CI.Function
             (CI.Primitive CI.Int,
              CI.Function (CI.Pointer x364, CI.Returns (CI.Pointer x365)))))) ->
  (fun x355 x357 x359 x362 x363 ->
    CI.make_ptr x365
      (gccjit_stub__28_gcc_jit_context_new_struct_type (CI.cptr x355)
       (CI.cptr x357) (CI.cptr (x360 x359)) x362 (CI.cptr x363)))
| "gcc_jit_field_as_object",
  CI.Function (CI.Pointer x367, CI.Returns (CI.Pointer x368)) ->
  (fun x366 ->
    CI.make_ptr x368 (gccjit_stub__27_gcc_jit_field_as_object (CI.cptr x366)))
| "gcc_jit_context_new_field",
  CI.Function
    (CI.Pointer x370,
     CI.Function
       (CI.Pointer x372,
        CI.Function
          (CI.Pointer x374,
           CI.Function
             (CI.View {CI.ty = CI.Pointer x377; write = x376; },
              CI.Returns (CI.Pointer x378))))) ->
  (fun x369 x371 x373 x375 ->
    CI.make_ptr x378
      (gccjit_stub__26_gcc_jit_context_new_field (CI.cptr x369)
       (CI.cptr x371) (CI.cptr x373) (CI.cptr (x376 x375))))
| "gcc_jit_context_new_array_type",
  CI.Function
    (CI.Pointer x380,
     CI.Function
       (CI.Pointer x382,
        CI.Function
          (CI.Pointer x384,
           CI.Function (CI.Primitive CI.Int, CI.Returns (CI.Pointer x386))))) ->
  (fun x379 x381 x383 x385 ->
    CI.make_ptr x386
      (gccjit_stub__25_gcc_jit_context_new_array_type (CI.cptr x379)
       (CI.cptr x381) (CI.cptr x383) x385))
| "gcc_jit_type_get_volatile",
  CI.Function (CI.Pointer x388, CI.Returns (CI.Pointer x389)) ->
  (fun x387 ->
    CI.make_ptr x389
      (gccjit_stub__24_gcc_jit_type_get_volatile (CI.cptr x387)))
| "gcc_jit_type_get_const",
  CI.Function (CI.Pointer x391, CI.Returns (CI.Pointer x392)) ->
  (fun x390 ->
    CI.make_ptr x392 (gccjit_stub__23_gcc_jit_type_get_const (CI.cptr x390)))
| "gcc_jit_type_get_pointer",
  CI.Function (CI.Pointer x394, CI.Returns (CI.Pointer x395)) ->
  (fun x393 ->
    CI.make_ptr x395
      (gccjit_stub__22_gcc_jit_type_get_pointer (CI.cptr x393)))
| "gcc_jit_context_get_int_type",
  CI.Function
    (CI.Pointer x397,
     CI.Function
       (CI.Primitive CI.Int,
        CI.Function (CI.Primitive CI.Int, CI.Returns (CI.Pointer x400)))) ->
  (fun x396 x398 x399 ->
    CI.make_ptr x400
      (gccjit_stub__21_gcc_jit_context_get_int_type (CI.cptr x396) x398 x399))
| "gcc_jit_context_get_type",
  CI.Function
    (CI.Pointer x402,
     CI.Function
       (CI.View {CI.ty = CI.Primitive CI.Uint32_t; write = x404; },
        CI.Returns (CI.Pointer x405))) ->
  (fun x401 x403 ->
    CI.make_ptr x405
      (gccjit_stub__20_gcc_jit_context_get_type (CI.cptr x401) (x404 x403)))
| "gcc_jit_type_as_object",
  CI.Function (CI.Pointer x407, CI.Returns (CI.Pointer x408)) ->
  (fun x406 ->
    CI.make_ptr x408 (gccjit_stub__19_gcc_jit_type_as_object (CI.cptr x406)))
| "gcc_jit_location_as_object",
  CI.Function (CI.Pointer x410, CI.Returns (CI.Pointer x411)) ->
  (fun x409 ->
    CI.make_ptr x411
      (gccjit_stub__18_gcc_jit_location_as_object (CI.cptr x409)))
| "gcc_jit_context_new_location",
  CI.Function
    (CI.Pointer x413,
     CI.Function
       (CI.View {CI.ty = CI.Pointer x416; write = x415; },
        CI.Function
          (CI.Primitive CI.Int,
           CI.Function (CI.Primitive CI.Int, CI.Returns (CI.Pointer x419))))) ->
  (fun x412 x414 x417 x418 ->
    CI.make_ptr x419
      (gccjit_stub__17_gcc_jit_context_new_location (CI.cptr x412)
       (CI.cptr (x415 x414)) x417 x418))
| "gcc_jit_object_get_debug_string",
  CI.Function
    (CI.Pointer x421,
     CI.Returns (CI.View {CI.ty = CI.Pointer x422; read = x423; })) ->
  (fun x420 ->
    x423
    (CI.make_ptr x422
       (gccjit_stub__16_gcc_jit_object_get_debug_string (CI.cptr x420))))
| "gcc_jit_object_get_context",
  CI.Function (CI.Pointer x425, CI.Returns (CI.Pointer x426)) ->
  (fun x424 ->
    CI.make_ptr x426
      (gccjit_stub__15_gcc_jit_object_get_context (CI.cptr x424)))
| "gcc_jit_result_release", CI.Function (CI.Pointer x428, CI.Returns CI.Void) ->
  (fun x427 -> gccjit_stub__14_gcc_jit_result_release (CI.cptr x427))
| "gcc_jit_get_global",
  CI.Function
    (CI.Pointer x430,
     CI.Function
       (CI.View {CI.ty = CI.Pointer x433; write = x432; },
        CI.Returns (CI.Pointer x434))) ->
  (fun x429 x431 ->
    CI.make_ptr x434
      (gccjit_stub__13_gcc_jit_get_global (CI.cptr x429)
        (CI.cptr (x432 x431))))
| "gcc_jit_result_get_code",
  CI.Function
    (CI.Pointer x436,
     CI.Function
       (CI.View {CI.ty = CI.Pointer x439; write = x438; },
        CI.Returns (CI.Pointer x440))) ->
  (fun x435 x437 ->
    CI.make_ptr x440
      (gccjit_stub__12_gcc_jit_result_get_code (CI.cptr x435)
        (CI.cptr (x438 x437))))
| "gcc_jit_context_get_last_error",
  CI.Function
    (CI.Pointer x442,
     CI.Returns (CI.View {CI.ty = CI.Pointer x443; read = x444; })) ->
  (fun x441 ->
    x444
    (CI.make_ptr x443
       (gccjit_stub__11_gcc_jit_context_get_last_error (CI.cptr x441))))
| "gcc_jit_context_get_first_error",
  CI.Function
    (CI.Pointer x446,
     CI.Returns (CI.View {CI.ty = CI.Pointer x447; read = x448; })) ->
  (fun x445 ->
    x448
    (CI.make_ptr x447
       (gccjit_stub__10_gcc_jit_context_get_first_error (CI.cptr x445))))
| "gcc_jit_context_set_logfile",
  CI.Function
    (CI.Pointer x450,
     CI.Function
       (CI.Pointer x452,
        CI.Function
          (CI.Primitive CI.Int,
           CI.Function (CI.Primitive CI.Int, CI.Returns CI.Void)))) ->
  (fun x449 x451 x453 x454 ->
    gccjit_stub__9_gcc_jit_context_set_logfile (CI.cptr x449) (CI.cptr x451)
    x453 x454)
| "gcc_jit_context_dump_to_file",
  CI.Function
    (CI.Pointer x456,
     CI.Function
       (CI.View {CI.ty = CI.Pointer x459; write = x458; },
        CI.Function (CI.Primitive CI.Int, CI.Returns CI.Void))) ->
  (fun x455 x457 x460 ->
    gccjit_stub__8_gcc_jit_context_dump_to_file (CI.cptr x455)
    (CI.cptr (x458 x457)) x460)
| "gcc_jit_context_compile_to_file",
  CI.Function
    (CI.Pointer x462,
     CI.Function
       (CI.View {CI.ty = CI.Primitive CI.Uint32_t; write = x464; },
        CI.Function
          (CI.View {CI.ty = CI.Pointer x467; write = x466; },
           CI.Returns CI.Void))) ->
  (fun x461 x463 x465 ->
    gccjit_stub__7_gcc_jit_context_compile_to_file (CI.cptr x461) (x464 x463)
    (CI.cptr (x466 x465)))
| "gcc_jit_context_compile",
  CI.Function (CI.Pointer x469, CI.Returns (CI.Pointer x470)) ->
  (fun x468 ->
    CI.make_ptr x470 (gccjit_stub__6_gcc_jit_context_compile (CI.cptr x468)))
| "gcc_jit_context_set_bool_option",
  CI.Function
    (CI.Pointer x472,
     CI.Function
       (CI.View {CI.ty = CI.Primitive CI.Uint32_t; write = x474; },
        CI.Function (CI.Primitive CI.Bool, CI.Returns CI.Void))) ->
  (fun x471 x473 x475 ->
    gccjit_stub__5_gcc_jit_context_set_bool_option (CI.cptr x471) (x474 x473)
    x475)
| "gcc_jit_context_set_int_option",
  CI.Function
    (CI.Pointer x477,
     CI.Function
       (CI.View {CI.ty = CI.Primitive CI.Uint32_t; write = x479; },
        CI.Function (CI.Primitive CI.Int, CI.Returns CI.Void))) ->
  (fun x476 x478 x480 ->
    gccjit_stub__4_gcc_jit_context_set_int_option (CI.cptr x476) (x479 x478)
    x480)
| "gcc_jit_context_set_str_option",
  CI.Function
    (CI.Pointer x482,
     CI.Function
       (CI.View {CI.ty = CI.Primitive CI.Uint32_t; write = x484; },
        CI.Function
          (CI.View {CI.ty = CI.Pointer x487; write = x486; },
           CI.Returns CI.Void))) ->
  (fun x481 x483 x485 ->
    gccjit_stub__3_gcc_jit_context_set_str_option (CI.cptr x481) (x484 x483)
    (CI.cptr (x486 x485)))
| "gcc_jit_context_release",
  CI.Function (CI.Pointer x489, CI.Returns CI.Void) ->
  (fun x488 -> gccjit_stub__2_gcc_jit_context_release (CI.cptr x488))
| "gcc_jit_context_acquire",
  CI.Function (CI.Void, CI.Returns (CI.Pointer x491)) ->
  (fun x490 ->
    CI.make_ptr x491 (gccjit_stub__1_gcc_jit_context_acquire x490))
| s, _ ->  Printf.ksprintf failwith "No match for %s" s


let foreign_value : type a b. string -> a Ctypes.typ -> a Ctypes.ptr =
  fun name t -> match name, t with
| s, _ ->  Printf.ksprintf failwith "No match for %s" s

