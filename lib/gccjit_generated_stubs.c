#include <gccjit.h>
#include "ctypes_cstubs_internals.h"

value gccjit_stub__1_gcc_jit_context_acquire(value x1)
{
   struct gcc_jit_context* x2 = gcc_jit_context_acquire();
   return CTYPES_FROM_PTR(x2);
}
value gccjit_stub__2_gcc_jit_context_release(value x3)
{
   struct gcc_jit_context* x4 = CTYPES_ADDR_OF_FATPTR(x3);
   gcc_jit_context_release(x4);
   return Val_unit;
}
value gccjit_stub__3_gcc_jit_context_set_str_option(value x8, value x7,
                                                    value x6)
{
   struct gcc_jit_context* x9 = CTYPES_ADDR_OF_FATPTR(x8);
   uint32_t x10 = Uint32_val(x7);
   char* x13 = CTYPES_ADDR_OF_FATPTR(x6);
   gcc_jit_context_set_str_option(x9, x10, x13);
   return Val_unit;
}
value gccjit_stub__4_gcc_jit_context_set_int_option(value x17, value x16,
                                                    value x15)
{
   struct gcc_jit_context* x18 = CTYPES_ADDR_OF_FATPTR(x17);
   uint32_t x19 = Uint32_val(x16);
   int x22 = Int_val(x15);
   gcc_jit_context_set_int_option(x18, x19, x22);
   return Val_unit;
}
value gccjit_stub__5_gcc_jit_context_set_bool_option(value x28, value x27,
                                                     value x26)
{
   struct gcc_jit_context* x29 = CTYPES_ADDR_OF_FATPTR(x28);
   uint32_t x30 = Uint32_val(x27);
   _Bool x33 = Bool_val(x26);
   gcc_jit_context_set_bool_option(x29, x30, x33);
   return Val_unit;
}
value gccjit_stub__6_gcc_jit_context_compile(value x37)
{
   struct gcc_jit_context* x38 = CTYPES_ADDR_OF_FATPTR(x37);
   struct gcc_jit_result* x39 = gcc_jit_context_compile(x38);
   return CTYPES_FROM_PTR(x39);
}
value gccjit_stub__7_gcc_jit_context_compile_to_file(value x42, value x41,
                                                     value x40)
{
   struct gcc_jit_context* x43 = CTYPES_ADDR_OF_FATPTR(x42);
   uint32_t x44 = Uint32_val(x41);
   char* x47 = CTYPES_ADDR_OF_FATPTR(x40);
   gcc_jit_context_compile_to_file(x43, x44, x47);
   return Val_unit;
}
value gccjit_stub__8_gcc_jit_context_dump_to_file(value x51, value x50,
                                                  value x49)
{
   struct gcc_jit_context* x52 = CTYPES_ADDR_OF_FATPTR(x51);
   char* x53 = CTYPES_ADDR_OF_FATPTR(x50);
   int x54 = Int_val(x49);
   gcc_jit_context_dump_to_file(x52, x53, x54);
   return Val_unit;
}
value gccjit_stub__9_gcc_jit_context_set_logfile(value x61, value x60,
                                                 value x59, value x58)
{
   struct gcc_jit_context* x62 = CTYPES_ADDR_OF_FATPTR(x61);
   void* x63 = CTYPES_ADDR_OF_FATPTR(x60);
   int x64 = Int_val(x59);
   int x67 = Int_val(x58);
   gcc_jit_context_set_logfile(x62, x63, x64, x67);
   return Val_unit;
}
value gccjit_stub__10_gcc_jit_context_get_first_error(value x71)
{
   struct gcc_jit_context* x72 = CTYPES_ADDR_OF_FATPTR(x71);
   char* x73 = gcc_jit_context_get_first_error(x72);
   return CTYPES_FROM_PTR(x73);
}
value gccjit_stub__11_gcc_jit_context_get_last_error(value x74)
{
   struct gcc_jit_context* x75 = CTYPES_ADDR_OF_FATPTR(x74);
   char* x76 = gcc_jit_context_get_last_error(x75);
   return CTYPES_FROM_PTR(x76);
}
value gccjit_stub__12_gcc_jit_result_get_code(value x78, value x77)
{
   struct gcc_jit_result* x79 = CTYPES_ADDR_OF_FATPTR(x78);
   char* x80 = CTYPES_ADDR_OF_FATPTR(x77);
   void* x81 = gcc_jit_result_get_code(x79, x80);
   return CTYPES_FROM_PTR(x81);
}
value gccjit_stub__13_gcc_jit_get_global(value x83, value x82)
{
   struct gcc_jit_result* x84 = CTYPES_ADDR_OF_FATPTR(x83);
   char* x85 = CTYPES_ADDR_OF_FATPTR(x82);
   void* x86 = gcc_jit_get_global(x84, x85);
   return CTYPES_FROM_PTR(x86);
}
value gccjit_stub__14_gcc_jit_result_release(value x87)
{
   struct gcc_jit_result* x88 = CTYPES_ADDR_OF_FATPTR(x87);
   gcc_jit_result_release(x88);
   return Val_unit;
}
value gccjit_stub__15_gcc_jit_object_get_context(value x90)
{
   struct gcc_jit_object* x91 = CTYPES_ADDR_OF_FATPTR(x90);
   struct gcc_jit_context* x92 = gcc_jit_object_get_context(x91);
   return CTYPES_FROM_PTR(x92);
}
value gccjit_stub__16_gcc_jit_object_get_debug_string(value x93)
{
   struct gcc_jit_object* x94 = CTYPES_ADDR_OF_FATPTR(x93);
   char* x95 = gcc_jit_object_get_debug_string(x94);
   return CTYPES_FROM_PTR(x95);
}
value gccjit_stub__17_gcc_jit_context_new_location(value x99, value x98,
                                                   value x97, value x96)
{
   struct gcc_jit_context* x100 = CTYPES_ADDR_OF_FATPTR(x99);
   char* x101 = CTYPES_ADDR_OF_FATPTR(x98);
   int x102 = Int_val(x97);
   int x105 = Int_val(x96);
   struct gcc_jit_location* x108 =
   gcc_jit_context_new_location(x100, x101, x102, x105);
   return CTYPES_FROM_PTR(x108);
}
value gccjit_stub__18_gcc_jit_location_as_object(value x109)
{
   struct gcc_jit_location* x110 = CTYPES_ADDR_OF_FATPTR(x109);
   struct gcc_jit_object* x111 = gcc_jit_location_as_object(x110);
   return CTYPES_FROM_PTR(x111);
}
value gccjit_stub__19_gcc_jit_type_as_object(value x112)
{
   struct gcc_jit_type* x113 = CTYPES_ADDR_OF_FATPTR(x112);
   struct gcc_jit_object* x114 = gcc_jit_type_as_object(x113);
   return CTYPES_FROM_PTR(x114);
}
value gccjit_stub__20_gcc_jit_context_get_type(value x116, value x115)
{
   struct gcc_jit_context* x117 = CTYPES_ADDR_OF_FATPTR(x116);
   uint32_t x118 = Uint32_val(x115);
   struct gcc_jit_type* x121 = gcc_jit_context_get_type(x117, x118);
   return CTYPES_FROM_PTR(x121);
}
value gccjit_stub__21_gcc_jit_context_get_int_type(value x124, value x123,
                                                   value x122)
{
   struct gcc_jit_context* x125 = CTYPES_ADDR_OF_FATPTR(x124);
   int x126 = Int_val(x123);
   int x129 = Int_val(x122);
   struct gcc_jit_type* x132 =
   gcc_jit_context_get_int_type(x125, x126, x129);
   return CTYPES_FROM_PTR(x132);
}
value gccjit_stub__22_gcc_jit_type_get_pointer(value x133)
{
   struct gcc_jit_type* x134 = CTYPES_ADDR_OF_FATPTR(x133);
   struct gcc_jit_type* x135 = gcc_jit_type_get_pointer(x134);
   return CTYPES_FROM_PTR(x135);
}
value gccjit_stub__23_gcc_jit_type_get_const(value x136)
{
   struct gcc_jit_type* x137 = CTYPES_ADDR_OF_FATPTR(x136);
   struct gcc_jit_type* x138 = gcc_jit_type_get_const(x137);
   return CTYPES_FROM_PTR(x138);
}
value gccjit_stub__24_gcc_jit_type_get_volatile(value x139)
{
   struct gcc_jit_type* x140 = CTYPES_ADDR_OF_FATPTR(x139);
   struct gcc_jit_type* x141 = gcc_jit_type_get_volatile(x140);
   return CTYPES_FROM_PTR(x141);
}
value gccjit_stub__25_gcc_jit_context_new_array_type(value x145, value x144,
                                                     value x143, value x142)
{
   struct gcc_jit_context* x146 = CTYPES_ADDR_OF_FATPTR(x145);
   struct gcc_jit_location* x147 = CTYPES_ADDR_OF_FATPTR(x144);
   struct gcc_jit_type* x148 = CTYPES_ADDR_OF_FATPTR(x143);
   int x149 = Int_val(x142);
   struct gcc_jit_type* x152 =
   gcc_jit_context_new_array_type(x146, x147, x148, x149);
   return CTYPES_FROM_PTR(x152);
}
value gccjit_stub__26_gcc_jit_context_new_field(value x156, value x155,
                                                value x154, value x153)
{
   struct gcc_jit_context* x157 = CTYPES_ADDR_OF_FATPTR(x156);
   struct gcc_jit_location* x158 = CTYPES_ADDR_OF_FATPTR(x155);
   struct gcc_jit_type* x159 = CTYPES_ADDR_OF_FATPTR(x154);
   char* x160 = CTYPES_ADDR_OF_FATPTR(x153);
   struct gcc_jit_field* x161 =
   gcc_jit_context_new_field(x157, x158, x159, x160);
   return CTYPES_FROM_PTR(x161);
}
value gccjit_stub__27_gcc_jit_field_as_object(value x162)
{
   struct gcc_jit_field* x163 = CTYPES_ADDR_OF_FATPTR(x162);
   struct gcc_jit_object* x164 = gcc_jit_field_as_object(x163);
   return CTYPES_FROM_PTR(x164);
}
value gccjit_stub__28_gcc_jit_context_new_struct_type(value x169, value x168,
                                                      value x167, value x166,
                                                      value x165)
{
   struct gcc_jit_context* x170 = CTYPES_ADDR_OF_FATPTR(x169);
   struct gcc_jit_location* x171 = CTYPES_ADDR_OF_FATPTR(x168);
   char* x172 = CTYPES_ADDR_OF_FATPTR(x167);
   int x173 = Int_val(x166);
   struct gcc_jit_field** x176 = CTYPES_ADDR_OF_FATPTR(x165);
   struct gcc_jit_struct* x177 =
   gcc_jit_context_new_struct_type(x170, x171, x172, x173, x176);
   return CTYPES_FROM_PTR(x177);
}
value gccjit_stub__29_gcc_jit_context_new_opaque_struct(value x180,
                                                        value x179,
                                                        value x178)
{
   struct gcc_jit_context* x181 = CTYPES_ADDR_OF_FATPTR(x180);
   struct gcc_jit_location* x182 = CTYPES_ADDR_OF_FATPTR(x179);
   char* x183 = CTYPES_ADDR_OF_FATPTR(x178);
   struct gcc_jit_struct* x184 =
   gcc_jit_context_new_opaque_struct(x181, x182, x183);
   return CTYPES_FROM_PTR(x184);
}
value gccjit_stub__30_gcc_jit_struct_as_type(value x185)
{
   struct gcc_jit_struct* x186 = CTYPES_ADDR_OF_FATPTR(x185);
   struct gcc_jit_type* x187 = gcc_jit_struct_as_type(x186);
   return CTYPES_FROM_PTR(x187);
}
value gccjit_stub__31_gcc_jit_struct_set_fields(value x191, value x190,
                                                value x189, value x188)
{
   struct gcc_jit_struct* x192 = CTYPES_ADDR_OF_FATPTR(x191);
   struct gcc_jit_location* x193 = CTYPES_ADDR_OF_FATPTR(x190);
   int x194 = Int_val(x189);
   struct gcc_jit_field** x197 = CTYPES_ADDR_OF_FATPTR(x188);
   gcc_jit_struct_set_fields(x192, x193, x194, x197);
   return Val_unit;
}
value gccjit_stub__32_gcc_jit_context_new_union_type(value x203, value x202,
                                                     value x201, value x200,
                                                     value x199)
{
   struct gcc_jit_context* x204 = CTYPES_ADDR_OF_FATPTR(x203);
   struct gcc_jit_location* x205 = CTYPES_ADDR_OF_FATPTR(x202);
   char* x206 = CTYPES_ADDR_OF_FATPTR(x201);
   int x207 = Int_val(x200);
   struct gcc_jit_field** x210 = CTYPES_ADDR_OF_FATPTR(x199);
   struct gcc_jit_type* x211 =
   gcc_jit_context_new_union_type(x204, x205, x206, x207, x210);
   return CTYPES_FROM_PTR(x211);
}
value gccjit_stub__33_gcc_jit_context_new_function_ptr_type(value x217,
                                                            value x216,
                                                            value x215,
                                                            value x214,
                                                            value x213,
                                                            value x212)
{
   struct gcc_jit_context* x218 = CTYPES_ADDR_OF_FATPTR(x217);
   struct gcc_jit_location* x219 = CTYPES_ADDR_OF_FATPTR(x216);
   struct gcc_jit_type* x220 = CTYPES_ADDR_OF_FATPTR(x215);
   int x221 = Int_val(x214);
   struct gcc_jit_type** x224 = CTYPES_ADDR_OF_FATPTR(x213);
   int x225 = Int_val(x212);
   struct gcc_jit_type* x228 =
   gcc_jit_context_new_function_ptr_type(x218, x219, x220, x221, x224, x225);
   return CTYPES_FROM_PTR(x228);
}
value gccjit_stub__33_gcc_jit_context_new_function_ptr_type_byte6(value* argv,
                                                                  int argc)
{
   value x229 = argv[5];
   value x230 = argv[4];
   value x231 = argv[3];
   value x232 = argv[2];
   value x233 = argv[1];
   value x234 = argv[0];
   return
     gccjit_stub__33_gcc_jit_context_new_function_ptr_type(x234, x233, 
                                                           x232, x231, 
                                                           x230, x229);
}
value gccjit_stub__34_gcc_jit_context_new_param(value x238, value x237,
                                                value x236, value x235)
{
   struct gcc_jit_context* x239 = CTYPES_ADDR_OF_FATPTR(x238);
   struct gcc_jit_location* x240 = CTYPES_ADDR_OF_FATPTR(x237);
   struct gcc_jit_type* x241 = CTYPES_ADDR_OF_FATPTR(x236);
   char* x242 = CTYPES_ADDR_OF_FATPTR(x235);
   struct gcc_jit_param* x243 =
   gcc_jit_context_new_param(x239, x240, x241, x242);
   return CTYPES_FROM_PTR(x243);
}
value gccjit_stub__35_gcc_jit_param_as_object(value x244)
{
   struct gcc_jit_param* x245 = CTYPES_ADDR_OF_FATPTR(x244);
   struct gcc_jit_object* x246 = gcc_jit_param_as_object(x245);
   return CTYPES_FROM_PTR(x246);
}
value gccjit_stub__36_gcc_jit_param_as_lvalue(value x247)
{
   struct gcc_jit_param* x248 = CTYPES_ADDR_OF_FATPTR(x247);
   struct gcc_jit_lvalue* x249 = gcc_jit_param_as_lvalue(x248);
   return CTYPES_FROM_PTR(x249);
}
value gccjit_stub__37_gcc_jit_param_as_rvalue(value x250)
{
   struct gcc_jit_param* x251 = CTYPES_ADDR_OF_FATPTR(x250);
   struct gcc_jit_rvalue* x252 = gcc_jit_param_as_rvalue(x251);
   return CTYPES_FROM_PTR(x252);
}
value gccjit_stub__38_gcc_jit_context_new_function(value x260, value x259,
                                                   value x258, value x257,
                                                   value x256, value x255,
                                                   value x254, value x253)
{
   struct gcc_jit_context* x261 = CTYPES_ADDR_OF_FATPTR(x260);
   struct gcc_jit_location* x262 = CTYPES_ADDR_OF_FATPTR(x259);
   uint32_t x263 = Uint32_val(x258);
   struct gcc_jit_type* x266 = CTYPES_ADDR_OF_FATPTR(x257);
   char* x267 = CTYPES_ADDR_OF_FATPTR(x256);
   int x268 = Int_val(x255);
   struct gcc_jit_param** x271 = CTYPES_ADDR_OF_FATPTR(x254);
   int x272 = Int_val(x253);
   struct gcc_jit_function* x275 =
   gcc_jit_context_new_function(x261, x262, x263, x266, x267, x268, x271,
                                x272);
   return CTYPES_FROM_PTR(x275);
}
value gccjit_stub__38_gcc_jit_context_new_function_byte8(value* argv,
                                                         int argc)
{
   value x276 = argv[7];
   value x277 = argv[6];
   value x278 = argv[5];
   value x279 = argv[4];
   value x280 = argv[3];
   value x281 = argv[2];
   value x282 = argv[1];
   value x283 = argv[0];
   return
     gccjit_stub__38_gcc_jit_context_new_function(x283, x282, x281, x280,
                                                  x279, x278, x277, x276);
}
value gccjit_stub__39_gcc_jit_context_get_builtin_function(value x285,
                                                           value x284)
{
   struct gcc_jit_context* x286 = CTYPES_ADDR_OF_FATPTR(x285);
   char* x287 = CTYPES_ADDR_OF_FATPTR(x284);
   struct gcc_jit_function* x288 =
   gcc_jit_context_get_builtin_function(x286, x287);
   return CTYPES_FROM_PTR(x288);
}
value gccjit_stub__40_gcc_jit_function_as_object(value x289)
{
   struct gcc_jit_function* x290 = CTYPES_ADDR_OF_FATPTR(x289);
   struct gcc_jit_object* x291 = gcc_jit_function_as_object(x290);
   return CTYPES_FROM_PTR(x291);
}
value gccjit_stub__41_gcc_jit_function_get_param(value x293, value x292)
{
   struct gcc_jit_function* x294 = CTYPES_ADDR_OF_FATPTR(x293);
   int x295 = Int_val(x292);
   struct gcc_jit_param* x298 = gcc_jit_function_get_param(x294, x295);
   return CTYPES_FROM_PTR(x298);
}
value gccjit_stub__42_gcc_jit_function_dump_to_dot(value x300, value x299)
{
   struct gcc_jit_function* x301 = CTYPES_ADDR_OF_FATPTR(x300);
   char* x302 = CTYPES_ADDR_OF_FATPTR(x299);
   gcc_jit_function_dump_to_dot(x301, x302);
   return Val_unit;
}
value gccjit_stub__43_gcc_jit_functin_new_block(value x305, value x304)
{
   struct gcc_jit_function* x306 = CTYPES_ADDR_OF_FATPTR(x305);
   char* x307 = CTYPES_ADDR_OF_FATPTR(x304);
   struct gcc_jit_block* x308 = gcc_jit_functin_new_block(x306, x307);
   return CTYPES_FROM_PTR(x308);
}
value gccjit_stub__44_gcc_jit_block_as_object(value x309)
{
   struct gcc_jit_block* x310 = CTYPES_ADDR_OF_FATPTR(x309);
   struct gcc_jit_object* x311 = gcc_jit_block_as_object(x310);
   return CTYPES_FROM_PTR(x311);
}
value gccjit_stub__45_gcc_jit_block_get_function(value x312)
{
   struct gcc_jit_block* x313 = CTYPES_ADDR_OF_FATPTR(x312);
   struct gcc_jit_function* x314 = gcc_jit_block_get_function(x313);
   return CTYPES_FROM_PTR(x314);
}
value gccjit_stub__46_gcc_jit_context_new_global(value x319, value x318,
                                                 value x317, value x316,
                                                 value x315)
{
   struct gcc_jit_context* x320 = CTYPES_ADDR_OF_FATPTR(x319);
   struct gcc_jit_location* x321 = CTYPES_ADDR_OF_FATPTR(x318);
   uint32_t x322 = Uint32_val(x317);
   struct gcc_jit_type* x325 = CTYPES_ADDR_OF_FATPTR(x316);
   char* x326 = CTYPES_ADDR_OF_FATPTR(x315);
   struct gcc_jit_lvalue* x327 =
   gcc_jit_context_new_global(x320, x321, x322, x325, x326);
   return CTYPES_FROM_PTR(x327);
}
value gccjit_stub__47_gcc_jit_lvalue_as_object(value x328)
{
   struct gcc_jit_lvalue* x329 = CTYPES_ADDR_OF_FATPTR(x328);
   struct gcc_jit_object* x330 = gcc_jit_lvalue_as_object(x329);
   return CTYPES_FROM_PTR(x330);
}
value gccjit_stub__48_gcc_jit_lvalue_as_rvalue(value x331)
{
   struct gcc_jit_lvalue* x332 = CTYPES_ADDR_OF_FATPTR(x331);
   struct gcc_jit_rvalue* x333 = gcc_jit_lvalue_as_rvalue(x332);
   return CTYPES_FROM_PTR(x333);
}
value gccjit_stub__49_gcc_jit_rvalue_as_object(value x334)
{
   struct gcc_jit_rvalue* x335 = CTYPES_ADDR_OF_FATPTR(x334);
   struct gcc_jit_object* x336 = gcc_jit_rvalue_as_object(x335);
   return CTYPES_FROM_PTR(x336);
}
value gccjit_stub__50_gcc_jit_rvalue_get_type(value x337)
{
   struct gcc_jit_rvalue* x338 = CTYPES_ADDR_OF_FATPTR(x337);
   struct gcc_jit_type* x339 = gcc_jit_rvalue_get_type(x338);
   return CTYPES_FROM_PTR(x339);
}
value gccjit_stub__51_gcc_jit_context_new_rvalue_from_int(value x342,
                                                          value x341,
                                                          value x340)
{
   struct gcc_jit_context* x343 = CTYPES_ADDR_OF_FATPTR(x342);
   struct gcc_jit_type* x344 = CTYPES_ADDR_OF_FATPTR(x341);
   int x345 = Int_val(x340);
   struct gcc_jit_rvalue* x348 =
   gcc_jit_context_new_rvalue_from_int(x343, x344, x345);
   return CTYPES_FROM_PTR(x348);
}
value gccjit_stub__52_gcc_jit_context_new_rvalue_from_long(value x351,
                                                           value x350,
                                                           value x349)
{
   struct gcc_jit_context* x352 = CTYPES_ADDR_OF_FATPTR(x351);
   struct gcc_jit_type* x353 = CTYPES_ADDR_OF_FATPTR(x350);
   int x354 = Int_val(x349);
   struct gcc_jit_rvalue* x357 =
   gcc_jit_context_new_rvalue_from_long(x352, x353, x354);
   return CTYPES_FROM_PTR(x357);
}
value gccjit_stub__53_gcc_jit_context_zero(value x359, value x358)
{
   struct gcc_jit_context* x360 = CTYPES_ADDR_OF_FATPTR(x359);
   struct gcc_jit_type* x361 = CTYPES_ADDR_OF_FATPTR(x358);
   struct gcc_jit_rvalue* x362 = gcc_jit_context_zero(x360, x361);
   return CTYPES_FROM_PTR(x362);
}
value gccjit_stub__54_gcc_jit_context_one(value x364, value x363)
{
   struct gcc_jit_context* x365 = CTYPES_ADDR_OF_FATPTR(x364);
   struct gcc_jit_type* x366 = CTYPES_ADDR_OF_FATPTR(x363);
   struct gcc_jit_rvalue* x367 = gcc_jit_context_one(x365, x366);
   return CTYPES_FROM_PTR(x367);
}
value gccjit_stub__55_gcc_jit_context_new_rvalue_from_double(value x370,
                                                             value x369,
                                                             value x368)
{
   struct gcc_jit_context* x371 = CTYPES_ADDR_OF_FATPTR(x370);
   struct gcc_jit_type* x372 = CTYPES_ADDR_OF_FATPTR(x369);
   double x373 = Double_val(x368);
   struct gcc_jit_rvalue* x376 =
   gcc_jit_context_new_rvalue_from_double(x371, x372, (float)x373);
   return CTYPES_FROM_PTR(x376);
}
value gccjit_stub__56_gcc_jit_context_new_rvalue_from_ptr(value x379,
                                                          value x378,
                                                          value x377)
{
   struct gcc_jit_context* x380 = CTYPES_ADDR_OF_FATPTR(x379);
   struct gcc_jit_type* x381 = CTYPES_ADDR_OF_FATPTR(x378);
   void* x382 = CTYPES_ADDR_OF_FATPTR(x377);
   struct gcc_jit_rvalue* x383 =
   gcc_jit_context_new_rvalue_from_ptr(x380, x381, x382);
   return CTYPES_FROM_PTR(x383);
}
value gccjit_stub__57_gcc_jit_context_null(value x385, value x384)
{
   struct gcc_jit_context* x386 = CTYPES_ADDR_OF_FATPTR(x385);
   struct gcc_jit_type* x387 = CTYPES_ADDR_OF_FATPTR(x384);
   struct gcc_jit_rvalue* x388 = gcc_jit_context_null(x386, x387);
   return CTYPES_FROM_PTR(x388);
}
value gccjit_stub__58_gcc_jit_context_new_string_literal(value x390,
                                                         value x389)
{
   struct gcc_jit_context* x391 = CTYPES_ADDR_OF_FATPTR(x390);
   char* x392 = CTYPES_ADDR_OF_FATPTR(x389);
   struct gcc_jit_rvalue* x393 =
   gcc_jit_context_new_string_literal(x391, x392);
   return CTYPES_FROM_PTR(x393);
}
value gccjit_stub__59_gcc_jit_context_new_unary_op(value x398, value x397,
                                                   value x396, value x395,
                                                   value x394)
{
   struct gcc_jit_context* x399 = CTYPES_ADDR_OF_FATPTR(x398);
   struct gcc_jit_location* x400 = CTYPES_ADDR_OF_FATPTR(x397);
   uint32_t x401 = Uint32_val(x396);
   struct gcc_jit_type* x404 = CTYPES_ADDR_OF_FATPTR(x395);
   struct gcc_jit_rvalue* x405 = CTYPES_ADDR_OF_FATPTR(x394);
   struct gcc_jit_rvalue* x406 =
   gcc_jit_context_new_unary_op(x399, x400, x401, x404, x405);
   return CTYPES_FROM_PTR(x406);
}
value gccjit_stub__60_gcc_jit_context_new_binary_op(value x412, value x411,
                                                    value x410, value x409,
                                                    value x408, value x407)
{
   struct gcc_jit_context* x413 = CTYPES_ADDR_OF_FATPTR(x412);
   struct gcc_jit_location* x414 = CTYPES_ADDR_OF_FATPTR(x411);
   uint32_t x415 = Uint32_val(x410);
   struct gcc_jit_type* x418 = CTYPES_ADDR_OF_FATPTR(x409);
   struct gcc_jit_rvalue* x419 = CTYPES_ADDR_OF_FATPTR(x408);
   struct gcc_jit_rvalue* x420 = CTYPES_ADDR_OF_FATPTR(x407);
   struct gcc_jit_rvalue* x421 =
   gcc_jit_context_new_binary_op(x413, x414, x415, x418, x419, x420);
   return CTYPES_FROM_PTR(x421);
}
value gccjit_stub__60_gcc_jit_context_new_binary_op_byte6(value* argv,
                                                          int argc)
{
   value x422 = argv[5];
   value x423 = argv[4];
   value x424 = argv[3];
   value x425 = argv[2];
   value x426 = argv[1];
   value x427 = argv[0];
   return
     gccjit_stub__60_gcc_jit_context_new_binary_op(x427, x426, x425, 
                                                   x424, x423, x422);
}
value gccjit_stub__61_gcc_jit_context_new_comparison(value x432, value x431,
                                                     value x430, value x429,
                                                     value x428)
{
   struct gcc_jit_context* x433 = CTYPES_ADDR_OF_FATPTR(x432);
   struct gcc_jit_location* x434 = CTYPES_ADDR_OF_FATPTR(x431);
   uint32_t x435 = Uint32_val(x430);
   struct gcc_jit_rvalue* x438 = CTYPES_ADDR_OF_FATPTR(x429);
   struct gcc_jit_rvalue* x439 = CTYPES_ADDR_OF_FATPTR(x428);
   struct gcc_jit_rvalue* x440 =
   gcc_jit_context_new_comparison(x433, x434, x435, x438, x439);
   return CTYPES_FROM_PTR(x440);
}
value gccjit_stub__62_gcc_jit_context_new_call(value x445, value x444,
                                               value x443, value x442,
                                               value x441)
{
   struct gcc_jit_context* x446 = CTYPES_ADDR_OF_FATPTR(x445);
   struct gcc_jit_location* x447 = CTYPES_ADDR_OF_FATPTR(x444);
   struct gcc_jit_function* x448 = CTYPES_ADDR_OF_FATPTR(x443);
   int x449 = Int_val(x442);
   struct gcc_jit_rvalue** x452 = CTYPES_ADDR_OF_FATPTR(x441);
   struct gcc_jit_rvalue* x453 =
   gcc_jit_context_new_call(x446, x447, x448, x449, x452);
   return CTYPES_FROM_PTR(x453);
}
value gccjit_stub__63_gcc_jit_context_new_call_through_ptr(value x458,
                                                           value x457,
                                                           value x456,
                                                           value x455,
                                                           value x454)
{
   struct gcc_jit_context* x459 = CTYPES_ADDR_OF_FATPTR(x458);
   struct gcc_jit_location* x460 = CTYPES_ADDR_OF_FATPTR(x457);
   struct gcc_jit_rvalue* x461 = CTYPES_ADDR_OF_FATPTR(x456);
   int x462 = Int_val(x455);
   struct gcc_jit_rvalue** x465 = CTYPES_ADDR_OF_FATPTR(x454);
   struct gcc_jit_rvalue* x466 =
   gcc_jit_context_new_call_through_ptr(x459, x460, x461, x462, x465);
   return CTYPES_FROM_PTR(x466);
}
value gccjit_stub__64_gcc_jit_context_new_cast(value x470, value x469,
                                               value x468, value x467)
{
   struct gcc_jit_context* x471 = CTYPES_ADDR_OF_FATPTR(x470);
   struct gcc_jit_location* x472 = CTYPES_ADDR_OF_FATPTR(x469);
   struct gcc_jit_rvalue* x473 = CTYPES_ADDR_OF_FATPTR(x468);
   struct gcc_jit_type* x474 = CTYPES_ADDR_OF_FATPTR(x467);
   struct gcc_jit_rvalue* x475 =
   gcc_jit_context_new_cast(x471, x472, x473, x474);
   return CTYPES_FROM_PTR(x475);
}
value gccjit_stub__65_gcc_jit_context_new_array_access(value x479,
                                                       value x478,
                                                       value x477,
                                                       value x476)
{
   struct gcc_jit_context* x480 = CTYPES_ADDR_OF_FATPTR(x479);
   struct gcc_jit_location* x481 = CTYPES_ADDR_OF_FATPTR(x478);
   struct gcc_jit_rvalue* x482 = CTYPES_ADDR_OF_FATPTR(x477);
   struct gcc_jit_rvalue* x483 = CTYPES_ADDR_OF_FATPTR(x476);
   struct gcc_jit_lvalue* x484 =
   gcc_jit_context_new_array_access(x480, x481, x482, x483);
   return CTYPES_FROM_PTR(x484);
}
value gccjit_stub__66_gcc_jit_lvalue_access_field(value x487, value x486,
                                                  value x485)
{
   struct gcc_jit_lvalue* x488 = CTYPES_ADDR_OF_FATPTR(x487);
   struct gcc_jit_location* x489 = CTYPES_ADDR_OF_FATPTR(x486);
   struct gcc_jit_field* x490 = CTYPES_ADDR_OF_FATPTR(x485);
   struct gcc_jit_lvalue* x491 =
   gcc_jit_lvalue_access_field(x488, x489, x490);
   return CTYPES_FROM_PTR(x491);
}
value gccjit_stub__67_gcc_jit_rvalue_access_field(value x494, value x493,
                                                  value x492)
{
   struct gcc_jit_rvalue* x495 = CTYPES_ADDR_OF_FATPTR(x494);
   struct gcc_jit_location* x496 = CTYPES_ADDR_OF_FATPTR(x493);
   struct gcc_jit_field* x497 = CTYPES_ADDR_OF_FATPTR(x492);
   struct gcc_jit_rvalue* x498 =
   gcc_jit_rvalue_access_field(x495, x496, x497);
   return CTYPES_FROM_PTR(x498);
}
value gccjit_stub__68_gcc_jit_rvalue_dereference_field(value x501,
                                                       value x500,
                                                       value x499)
{
   struct gcc_jit_rvalue* x502 = CTYPES_ADDR_OF_FATPTR(x501);
   struct gcc_jit_location* x503 = CTYPES_ADDR_OF_FATPTR(x500);
   struct gcc_jit_field* x504 = CTYPES_ADDR_OF_FATPTR(x499);
   struct gcc_jit_lvalue* x505 =
   gcc_jit_rvalue_dereference_field(x502, x503, x504);
   return CTYPES_FROM_PTR(x505);
}
value gccjit_stub__69_gcc_jit_rvalue_dereference(value x507, value x506)
{
   struct gcc_jit_rvalue* x508 = CTYPES_ADDR_OF_FATPTR(x507);
   struct gcc_jit_location* x509 = CTYPES_ADDR_OF_FATPTR(x506);
   struct gcc_jit_lvalue* x510 = gcc_jit_rvalue_dereference(x508, x509);
   return CTYPES_FROM_PTR(x510);
}
value gccjit_stub__70_gcc_jit_lvalue_get_address(value x512, value x511)
{
   struct gcc_jit_lvalue* x513 = CTYPES_ADDR_OF_FATPTR(x512);
   struct gcc_jit_location* x514 = CTYPES_ADDR_OF_FATPTR(x511);
   struct gcc_jit_rvalue* x515 = gcc_jit_lvalue_get_address(x513, x514);
   return CTYPES_FROM_PTR(x515);
}
value gccjit_stub__71_gcc_jit_function_new_local(value x519, value x518,
                                                 value x517, value x516)
{
   struct gcc_jit_function* x520 = CTYPES_ADDR_OF_FATPTR(x519);
   struct gcc_jit_location* x521 = CTYPES_ADDR_OF_FATPTR(x518);
   struct gcc_jit_type* x522 = CTYPES_ADDR_OF_FATPTR(x517);
   char* x523 = CTYPES_ADDR_OF_FATPTR(x516);
   struct gcc_jit_lvalue* x524 =
   gcc_jit_function_new_local(x520, x521, x522, x523);
   return CTYPES_FROM_PTR(x524);
}
value gccjit_stub__72_gcc_jit_block_add_eval(value x527, value x526,
                                             value x525)
{
   struct gcc_jit_block* x528 = CTYPES_ADDR_OF_FATPTR(x527);
   struct gcc_jit_location* x529 = CTYPES_ADDR_OF_FATPTR(x526);
   struct gcc_jit_rvalue* x530 = CTYPES_ADDR_OF_FATPTR(x525);
   gcc_jit_block_add_eval(x528, x529, x530);
   return Val_unit;
}
value gccjit_stub__73_gcc_jit_block_add_assignment(value x535, value x534,
                                                   value x533, value x532)
{
   struct gcc_jit_block* x536 = CTYPES_ADDR_OF_FATPTR(x535);
   struct gcc_jit_location* x537 = CTYPES_ADDR_OF_FATPTR(x534);
   struct gcc_jit_lvalue* x538 = CTYPES_ADDR_OF_FATPTR(x533);
   struct gcc_jit_rvalue* x539 = CTYPES_ADDR_OF_FATPTR(x532);
   gcc_jit_block_add_assignment(x536, x537, x538, x539);
   return Val_unit;
}
value gccjit_stub__74_gcc_jit_block_add_assignment_op(value x545, value x544,
                                                      value x543, value x542,
                                                      value x541)
{
   struct gcc_jit_block* x546 = CTYPES_ADDR_OF_FATPTR(x545);
   struct gcc_jit_location* x547 = CTYPES_ADDR_OF_FATPTR(x544);
   struct gcc_jit_lvalue* x548 = CTYPES_ADDR_OF_FATPTR(x543);
   uint32_t x549 = Uint32_val(x542);
   struct gcc_jit_rvalue* x552 = CTYPES_ADDR_OF_FATPTR(x541);
   gcc_jit_block_add_assignment_op(x546, x547, x548, x549, x552);
   return Val_unit;
}
value gccjit_stub__75_gcc_jit_block_add_comment(value x556, value x555,
                                                value x554)
{
   struct gcc_jit_block* x557 = CTYPES_ADDR_OF_FATPTR(x556);
   struct gcc_jit_location* x558 = CTYPES_ADDR_OF_FATPTR(x555);
   char* x559 = CTYPES_ADDR_OF_FATPTR(x554);
   gcc_jit_block_add_comment(x557, x558, x559);
   return Val_unit;
}
value gccjit_stub__76_gcc_jit_block_end_with_conditional(value x565,
                                                         value x564,
                                                         value x563,
                                                         value x562,
                                                         value x561)
{
   struct gcc_jit_block* x566 = CTYPES_ADDR_OF_FATPTR(x565);
   struct gcc_jit_location* x567 = CTYPES_ADDR_OF_FATPTR(x564);
   struct gcc_jit_rvalue* x568 = CTYPES_ADDR_OF_FATPTR(x563);
   struct gcc_jit_block* x569 = CTYPES_ADDR_OF_FATPTR(x562);
   struct gcc_jit_block* x570 = CTYPES_ADDR_OF_FATPTR(x561);
   gcc_jit_block_end_with_conditional(x566, x567, x568, x569, x570);
   return Val_unit;
}
value gccjit_stub__77_gcc_jit_block_end_with_jump(value x574, value x573,
                                                  value x572)
{
   struct gcc_jit_block* x575 = CTYPES_ADDR_OF_FATPTR(x574);
   struct gcc_jit_location* x576 = CTYPES_ADDR_OF_FATPTR(x573);
   struct gcc_jit_block* x577 = CTYPES_ADDR_OF_FATPTR(x572);
   gcc_jit_block_end_with_jump(x575, x576, x577);
   return Val_unit;
}
value gccjit_stub__78_gcc_jit_block_end_with_return(value x581, value x580,
                                                    value x579)
{
   struct gcc_jit_block* x582 = CTYPES_ADDR_OF_FATPTR(x581);
   struct gcc_jit_location* x583 = CTYPES_ADDR_OF_FATPTR(x580);
   struct gcc_jit_rvalue* x584 = CTYPES_ADDR_OF_FATPTR(x579);
   gcc_jit_block_end_with_return(x582, x583, x584);
   return Val_unit;
}
value gccjit_stub__79_gcc_jit_block_end_with_void_return(value x587,
                                                         value x586)
{
   struct gcc_jit_block* x588 = CTYPES_ADDR_OF_FATPTR(x587);
   struct gcc_jit_location* x589 = CTYPES_ADDR_OF_FATPTR(x586);
   gcc_jit_block_end_with_void_return(x588, x589);
   return Val_unit;
}
value gccjit_stub__80_gcc_jit_context_new_child_context(value x591)
{
   struct gcc_jit_context* x592 = CTYPES_ADDR_OF_FATPTR(x591);
   struct gcc_jit_context* x593 = gcc_jit_context_new_child_context(x592);
   return CTYPES_FROM_PTR(x593);
}
value gccjit_stub__81_gcc_jit_context_dump_reproducer_to_file(value x595,
                                                              value x594)
{
   struct gcc_jit_context* x596 = CTYPES_ADDR_OF_FATPTR(x595);
   char* x597 = CTYPES_ADDR_OF_FATPTR(x594);
   gcc_jit_context_dump_reproducer_to_file(x596, x597);
   return Val_unit;
}
