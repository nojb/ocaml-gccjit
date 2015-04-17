(* Smoketest example for libgccjit.so *)

open Gccjit

(* Let's try to inject the equivalent of:

     void
     greet (const char *name)
     {
        printf ("hello %s\n", name);
     } *)
let create_code ctx =
  let param_name = Param.create ctx Type.(get ctx Const_char_ptr) "name" in
  let func = Function.create ctx Function.Exported Type.(get ctx Void) "greet" [ param_name ] in
  let param_format = Param.create ctx Type.(get ctx Const_char_ptr) "format" in
  let printf_func =
    Function.create ctx ~variadic:true Function.Imported Type.(get ctx Int) "printf" [ param_format ]
  in
  let hello = RValue.string_literal ctx "hello %s\n" in
  let block = Block.create func in
  Block.eval block (RValue.call ctx printf_func [ hello; RValue.param param_name ]);
  Block.return_void block

let () =
  let ctx = Context.create () in

  (* Set some options on the context.
     Let's see the code being generated, in assembler form. *)
  Context.set_option ctx Context.Dump_generated_code true;

  (* Populate the context. *)
  create_code ctx;

  (* Compile the code. *)
  let result = Context.compile ctx in

  (* Extract the generated code from "result". *)
  let greet = Result.code result "greet" Ctypes.(string @-> returning void) in

  (* Now call the generated function: *)
  greet "world";
  flush stdout;

  Context.release ctx;
  Result.release result
