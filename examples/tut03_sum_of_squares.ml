(* Usage example for libgccjit *)

module G = Gccjit.Make ()

open G

let create_code () =
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

  let n = Param.create Type.int "n" in
  let func = Function.create Function.Exported Type.int "loop_test" [ n ] in

  (* Build locals:  *)
  let i = Function.local func Type.int "i" in
  let sum = Function.local func Type.int "sum" in

  let b_initial = Block.create ~name:"initial" func in
  let b_loop_cond = Block.create ~name:"loop_cond" func in
  let b_loop_body = Block.create ~name:"loop_body" func in
  let b_after_loop = Block.create ~name:"after_loop" func in

  (* sum = 0; *)
  Block.assign b_initial sum (RValue.zero Type.int);

  (* i = 0; *)
  Block.assign b_initial i (RValue.zero Type.int);

  Block.jump b_initial b_loop_cond;

  (* if (i >= n) *)
  Block.cond_jump b_loop_cond (RValue.comparison Ge (RValue.lvalue i) (RValue.param n))
    b_after_loop b_loop_body;

  (* sum += i * i *)
  Block.assign_op b_loop_body sum Plus (RValue.binary_op Mult Type.int (RValue.lvalue i) (RValue.lvalue i));

  (* i++ *)
  Block.assign_op b_loop_body i Plus (RValue.one Type.int);

  Block.jump b_loop_body b_loop_cond;

  (* return sum *)
  Block.return b_after_loop (RValue.lvalue sum)

let () =
  (* Set some options on the context.  Let's see the code being generated, in
     assembler form. *)
  Context.set_option Context.Dump_generated_code true;

  (* Populate the context. *)
  create_code ();

  (* Compile the code. *)
  let result = Context.compile () in

  (* Extract the generated code from "result". *)
  let loop_test = Result.code result "loop_test" Ctypes.(int @-> returning int) in

  (* Run the generated code. *)
  let v = loop_test 10 in
  Printf.printf "loop_test returned: %d\n%!" v;

  Context.release ();
  Result.release result
