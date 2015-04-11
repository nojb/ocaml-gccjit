`ocaml-gccjit` is a OCaml library that provides bidings for
[`libgccjit`](https://gcc.gnu.org/wiki/JIT), the new JIT compiler in GCC5.

For example, consider this C function:

```c
int square (int i)
{
  return i * i;
}
```

We can construct this function at runtime using `libgccjit`, as follows:

```ocaml
open Gccjit

let square =
  (* Create a compilation context *)
  let ctx = acquire () in

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
  get_code jit_result "square" Ctypes.(int @-> returning int)
```

We can now call the function by doing simply
```ocaml
  (* Now try running the code *)
  Printf.printf "square(5) = %d\n%!" square 5
```

### Links

- [API documentation](https://nojb.github.io/ocaml-gccjit)
- [The C header file](https://github.com/gcc-mirror/gcc/blob/master/gcc/jit/libgccjit.h)
- [libgccjit wiki](https://gcc.gnu.org/wiki/JIT)
- [Experiments in JIT compilation](https://github.com/davidmalcolm/jittest)

### Contact

Nicolas Ojeda Bar: <n.oje.bar@gmail.com>
