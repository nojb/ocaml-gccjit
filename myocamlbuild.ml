open Ocamlbuild_plugin

let pkg_ctypes = Findlib.query "ctypes"
let cc = "cc" (* C compiler *)

let () =
  dispatch begin function
    | Before_options ->
        Options.use_ocamlfind := true
    | After_rules ->
        rule "gccjit c types generator"
          ~dep:"stubgen/gen_types_generator.byte"
          ~prod:"stubgen/gccjit_types_generator.c"
          (fun _ _ -> Cmd (S [P "stubgen/gen_types_generator.byte"; Sh ">"; A "stubgen/gccjit_types_generator.c"]));

        rule "gccjit bin types generator"
          ~dep:"stubgen/gccjit_types_generator.o"
          ~prod:"stubgen/gccjit_types_generator"
          (fun _ _ ->
             Cmd (S [P cc; A "-o"; P "stubgen/gccjit_types_generator"; A "stubgen/gccjit_types_generator.o"]));

        rule "gccjit ml generated types"
          ~dep:"stubgen/gccjit_types_generator"
          ~prod:"lib/gccjit_types_generated.ml"
          (fun _ _ -> Cmd (S [P "stubgen/gccjit_types_generator"; Sh ">"; A "lib/gccjit_types_generated.ml"]));

        rule "gccjit c & ml generated stubs"
          ~deps:["lib/gccjit_types_generated.ml"; "stubgen/gen_stubs.byte"]
          ~prods:["lib/gccjit_stubs_generated.ml"; "lib/gccjit_stubs.c"]
          (fun _ _ -> Cmd (S [P "stubgen/gen_stubs.byte"]));

        flag [ "compile"; "c"; "use_ctypes_c_headers" ] (S [A "-I"; P pkg_ctypes.Findlib.location]);
        flag [ "compile"; "c"; "use_ocaml_c_headers" ] (S [A "-I"; P (pkg_ctypes.Findlib.location ^ "/../ocaml")]);

        dep [ "ocaml"; "link"; "use_gccjit" ] [ "lib/gccjit_stubs.o" ];
        flag [ "ocaml"; "link"; "use_gccjit"; "native" ] (S [A"-cclib"; P"-lgccjit"])
    | _ ->
        ()
  end
