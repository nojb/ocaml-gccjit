(* Smoketest example for libgccjit.so *)

open Gccjit

(* Let's try to inject the equivalent of:

     void
     greet (const char *name)
     {
        printf ("hello %s\n", name);
     } *)
let create_code ctx =
  let void_type = get_standard_type ctx Void in
  let const_char_ptr_type = get_standard_type ctx Const_char_ptr in
  let param_name = new_param ctx const_char_ptr_type "name" in
  let func = new_function ctx Exported void_type "greet" [ param_name ] in
  let param_format = new_param ctx const_char_ptr_type "format" in
  let printf_func =
    new_function ctx ~variadic:true Imported (get_standard_type ctx Int) "printf" [ param_format ]
  in
  let hello = new_string_literal ctx "hello %s\n" in
  let block = new_block func () in
  add_eval block (new_call ctx printf_func [ hello; (param_name :> rvalue) ]);
  end_with_void_return block

let () =
  (* Get a "context" object for working with the library. *)
  let ctx = acquire_context () in

  (* Set some options on the context.
     Let's see the code being generated, in assembler form. *)
  set_option ctx Dump_generated_code false;

  (* Populate the context. *)
  create_code ctx;

  (* Compile the code. *)
  let result = compile ctx in

  (* Extract the generated code from "result". *)
  let greet = get_code result "greet" Ctypes.(string @-> returning void) in

  (* Now call the generated function: *)
  greet "world";
  flush stdout;

  release_context ctx;
  release_result result
