(* Usage example for libgccjit *)

open Gccjit

let create_code ctx =
  (*
    Simple sum-of-squares, to test conditionals and looping

    int loop_test (int n)
    {
      int i;
      int sum = 0;
      for (i = 0; i < n ; i ++)
      {
	sum += i * i;
      }
      return sum;
   *)

  let the_type = get_standard_type ctx Int in
  let return_type = the_type in
  let n = new_param ctx the_type "n" in
  let func = new_function ctx Exported return_type "loop_test" [ n ] in

  (* Build locals:  *)
  let i = new_local func the_type "i" in
  let sum = new_local func the_type "sum" in

  let b_initial = new_block func ~name:"initial" () in
  let b_loop_cond = new_block func ~name:"loop_cond" () in
  let b_loop_body = new_block func ~name:"loop_body" () in
  let b_after_loop = new_block func ~name:"after_loop" () in

  (* sum = 0; *)
  add_assignment b_initial sum (zero ctx the_type);

  (* i = 0; *)
  add_assignment b_initial i (zero ctx the_type);

  end_with_jump b_initial b_loop_cond;

  (* if (i >= n) *)
  end_with_conditional b_loop_cond (new_comparison ctx Ge i n) b_after_loop b_loop_body;

  (* sum += i * i *)
  add_assignment_op b_loop_body sum Plus (new_binary_op ctx Mult the_type i i);

  (* i++ *)
  add_assignment_op b_loop_body i Plus (one ctx the_type);

  end_with_jump b_loop_body b_loop_cond;

  (* return sum *)
  end_with_return b_after_loop sum

let () =
  try
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
    let loop_test = get_code result "loop_test" Ctypes.(int @-> returning int) in

    (* Run the generated code. *)
    let v = loop_test 10 in
    Printf.printf "loop_test returned: %d\n%!" v;

    release_context ctx;
    release_result result
  with
  | Error _ -> exit 1
