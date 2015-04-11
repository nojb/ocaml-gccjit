open Ocamlbuild_plugin

let ctypes_lib = let ctypes = Findlib.query "ctypes" in ctypes.Findlib.location
let ocaml_stdlib = run_and_read "ocamlfind printconf stdlib"
let cc = "cc" (* C compiler *)

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

        flag [ "compile"; "c"; "use_ctypes_c_headers" ] (S [A "-I"; P ctypes_lib]);
        flag [ "compile"; "c"; "use_ocaml_c_headers" ] (S [A "-I"; P ocaml_stdlib]);

        dep [ "ocaml"; "link"; "use_gccjit" ] [ "lib/gccjit_stubs.o" ];
        flag [ "ocaml"; "link"; "use_gccjit" ] (S [A"-cclib"; P"-lgccjit"]);
        flag [ "ocaml"; "link"; "byte"; "use_gccjit" ]  (S[A"-custom"])

    | _ ->
        ()
  end
