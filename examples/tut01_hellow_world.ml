(* Smoketest example for libgccjit.so *)

module G = Gccjit.Make ()

open G

(* Let's try to inject the equivalent of:

     void
     greet (const char *name)
     {
        printf ("hello %s\n", name);
     } *)
let create_code () =
  let param_name = Param.create Type.(standard Const_char_ptr) "name" in
  let func = Function.create Function.Exported Type.(standard Void) "greet" [ param_name ] in
  let param_format = Param.create Type.(standard Const_char_ptr) "format" in
  let printf_func = Function.create ~variadic:true Function.Imported Type.int "printf" [ param_format ] in
  let hello = RValue.string_literal "hello %s\n" in
  let block = Block.create func in
  Block.eval block (RValue.call printf_func [ hello; RValue.param param_name ]);
  Block.return_void block

let () =
  (* Set some options on the context.
     Let's see the code being generated, in assembler form. *)
  Context.set_option Context.Dump_generated_code true;

  (* Populate the context. *)
  create_code ();

  (* Compile the code. *)
  let result = Context.compile () in

  (* Extract the generated code from "result". *)
  let greet = Result.code result "greet" Ctypes.(string @-> returning void) in

  (* Now call the generated function: *)
  greet "world";
  flush stdout;

  Context.release ();
  Result.release result
