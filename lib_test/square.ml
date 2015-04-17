module G = Gccjit.Make ()

open G

let main () =
  (* Turn these on to get various kinds of debugging *)
  Context.set_option Context.Dump_initial_tree true;
  Context.set_option Context.Dump_initial_gimple true;
  Context.set_option Context.Dump_generated_code true;

  (* Adjust this to control optimization level of the generated code *)
  Context.set_option Context.Optimization_level 3;

  (* Create parameter "i" *)
  let param_i = Param.create Type.int "i" in

  (* Create the function *)
  let fn = Function.create Function.Exported Type.int "square" [ param_i ] in

  (* Create a basic block within the function *)
  let block = Block.create fn in

  (* This basic block is relatively simple *)
  let expr = RValue.binary_op Mult Type.int (RValue.param param_i) (RValue.param param_i) in

  Block.return block expr;

  (* Having populated the context, compile it *)
  let res = Context.compile () in

  (* Look up a specific machine code routine within the gccjit.Result, in this
     case, the function we created above: *)
  let callable = Result.code res "square" Ctypes.(int @-> returning int) in

  (* Now try running the code *)
  assert (25 = callable 5)

let _ =
  main ()
