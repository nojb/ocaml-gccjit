(* Usage example for libgccjit.so *)

open Gccjit

let create_code ctx =
  (* Let's try to inject the equivalent of:

      int square (int i)
      {
        return i * i;
      }
  *)
  let int_type = get_standard_type ctx Int in
  let param_i = new_param ctx int_type "i" in
  let func = new_function ctx Exported int_type "square" [ param_i ] in

  let block = new_block func () in

  let expr = new_binary_op ctx Mult int_type param_i param_i in

  end_with_return block expr

let () =
  (* Get a "context" object for working with the library. *)
  let ctx = acquire_context () in

  (* Set some options on the context.
     Let's see the code being generated, in assembler form.  *)
  set_option ctx Dump_generated_code false;

  (* Populate the context. *)
  create_code ctx;

  (* Compile the code. *)
  let result = compile ctx in

  (* We're done with the context; we can release it: *)
  release_context ctx;

  (* Extract the generated code from "result". *)
  let square = get_code result "square" Ctypes.(int @-> returning int) in
  Printf.printf "result: %d%!\n" (square 5);

  release_result result
