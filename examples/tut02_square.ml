(* Usage example for libgccjit.so *)

open Gccjit

let create_code ctx =
  (* Let's try to inject the equivalent of:

      int square (int i)
      {
        return i * i;
      }
  *)
  let param_i = Param.create ctx Type.(get ctx Int) "i" in
  let func = Function.create ctx Function.Exported Type.(get ctx Int) "square" [ param_i ] in
  let block = Block.create func in
  let expr = RValue.binary_op ctx Mult Type.(get ctx Int) (RValue.param param_i) (RValue.param param_i) in
  Block.return block expr

let () =
  let ctx = Context.create () in

  (* Set some options on the context.
     Let's see the code being generated, in assembler form.  *)
  Context.set_option ctx Context.Dump_generated_code true;

  (* Populate the context. *)
  create_code ctx;

  (* Compile the code. *)
  let result = Context.compile ctx in

  (* We're done with the context; we can release it: *)
  Context.release ctx;

  (* Extract the generated code from "result". *)
  let square = Result.code result "square" Ctypes.(int @-> returning int) in
  Printf.printf "result: %d%!\n" (square 5);

  Result.release result
