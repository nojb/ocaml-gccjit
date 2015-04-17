(* Usage example for libgccjit.so *)

module G = Gccjit.Make ()
open G

let create_code () =
  (* Let's try to inject the equivalent of:

      int square (int i)
      {
        return i * i;
      }
  *)
  let param_i = Param.create Type.int "i" in
  let func = Function.create Function.Exported Type.int "square" [ param_i ] in
  let block = Block.create func in
  let expr = RValue.binary_op Mult Type.int (RValue.param param_i) (RValue.param param_i) in
  Block.return block expr

let () =
  (* Set some options on the context.
     Let's see the code being generated, in assembler form.  *)
  Context.set_option Context.Dump_generated_code true;

  (* Populate the context. *)
  create_code ();

  (* Compile the code. *)
  let result = Context.compile () in

  (* We're done with the context; we can release it: *)
  Context.release ();

  (* Extract the generated code from "result". *)
  let square = Result.code result "square" Ctypes.(int @-> returning int) in
  Printf.printf "result: %d%!\n" (square 5);

  Result.release result
