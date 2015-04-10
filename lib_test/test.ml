let main () =
  let open Gccjit in
  let ctx = acquire () in
  let int_type = get_type ctx Int in
  let param_i = new_param ctx "i" int_type in
  let fn = new_function ctx Exported "square" [param_i] int_type in
  let block = new_block fn ~name:"entry" () in
  let expr = new_binary_op ctx Mult int_type param_i param_i in
  end_with_return block expr;
  let jit_result = compile ctx in
  let callable = get_code jit_result "square" Ctypes.(int @-> returning int) in
  Printf.printf "%d\n%!" (callable 5);
  set_option ctx Dump_initial_gimple true;
  let jit_result = compile ctx in
  set_option ctx Dump_generated_code true;
  let jit_result = compile ctx in
  set_option ctx Optimization_level 3;
  let jit_result = compile ctx in
  ()

let _ =
  main ()
