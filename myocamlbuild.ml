open Ocamlbuild_plugin

let ctypes_lib = let ctypes = Findlib.query "ctypes" in ctypes.Findlib.location
let ocaml_stdlib = run_and_read "ocamlfind printconf stdlib"
let cc = "cc" (* C compiler *)

let c_headers tag dir =
  flag [ "compile"; "c"; tag ] (S [ A "-I"; P dir ])

let libgccjit_dir =
  try S [A "-L"; P (Sys.getenv "LIBGCCJIT_DIR")] with Not_found -> N

let ldopt =
  try S [A "-ccopt"; A "-L"; A"-ccopt"; P (Sys.getenv "LIBGCCJIT_DIR")] with Not_found -> N

let () =
  dispatch begin function
    | Before_options ->
        Options.use_ocamlfind := true
    | After_rules ->
        rule "gccjit c types generator"
          ~dep:"lib_gen/gen_types_generator.byte"
          ~prod:"lib_gen/gccjit_types_generator.c"
          (fun _ _ -> Cmd (S [P "lib_gen/gen_types_generator.byte"; Sh ">"; A "lib_gen/gccjit_types_generator.c"]));

        rule "gccjit bin types generator"
          ~dep:"lib_gen/gccjit_types_generator.o"
          ~prod:"lib_gen/gccjit_types_generator"
          (fun _ _ ->
             Cmd (S [P cc; A "-o"; P "lib_gen/gccjit_types_generator"; A "lib_gen/gccjit_types_generator.o"]));

        rule "gccjit ml generated types"
          ~dep:"lib_gen/gccjit_types_generator"
          ~prod:"lib/gccjit_types_generated.ml"
          (fun _ _ -> Cmd (S [P "lib_gen/gccjit_types_generator"; Sh ">"; A "lib/gccjit_types_generated.ml"]));

        rule "gccjit c & ml generated stubs"
          ~deps:["lib/gccjit_types_generated.ml"; "lib_gen/gen_stubs.byte"]
          ~prods:["lib/gccjit_stubs_generated.ml"; "lib/gccjit_stubs.c"]
          (fun _ _ -> Cmd (S [P "lib_gen/gen_stubs.byte"]));

        c_headers "use_ctypes" ctypes_lib;
        c_headers "use_ocaml" ocaml_stdlib;

        flag [ "c"; "ocamlmklib"; "use_gccjit" ] (S [libgccjit_dir; A "-lgccjit"]);

        flag [ "ocaml"; "link"; "use_gccjit"; "byte" ] (S [A"-dllib"; A"-lgccjit_stubs"]);

        flag [ "ocaml"; "link"; "use_gccjit"; "library"; "native" ]
          (S [ A "-cclib"; A "-lgccjit_stubs"; ldopt; A "-cclib"; A"-lgccjit"]);

    | _ ->
        ()
  end
