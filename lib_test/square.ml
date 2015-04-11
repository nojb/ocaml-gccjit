open Gccjit

let main () =
  (* Create a compilation context *)
  let ctx = acquire () in

  (* Turn these on to get various kinds of debugging *)
  (* set_option ctx Dump_initial_tree true; *)
  (* set_option ctx Dump_initial_gimple true; *)
  (* set_option ctx Dump_generated_code true; *)

  (* Adjust this to control optimization level of the generated code *)
  (* set_option ctx Optimization_level 3; *)

  let int_type = get_standard_type ctx Int in

  (* Create parameter "i" *)
  let param_i = new_param ctx int_type "i" in

  (* Create the function *)
  let fn = new_function ctx Exported int_type "square" [ param_i ] in

  (* Create a basic block within the function *)
  let block = new_block fn ~name:"entry" () in

  (* This basic block is relatively simple *)
  let expr = new_binary_op ctx Mult int_type param_i param_i in
  end_with_return block expr;

  (* Having populated the context, compile it *)
  let jit_result = compile ctx in

  (* Look up a specific machine code routine within the gccjit.Result, in this
     case, the function we created above: *)
  let callable = get_code jit_result "square" Ctypes.(int @-> returning int) in

  (* Now try running the code *)
  assert (25 = callable 5)

let _ =
  main ()
