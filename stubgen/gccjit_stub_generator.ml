let c_headers = "#include <gccjit.h>"

module B = Gccjit_stubs.Bindings (Gccjit_enums_gen)

let main () =
  let ml_out = open_out "lib/gccjit_generated.ml"
  and c_out = open_out "lib/gccjit_generated_stubs.c" in
  let ml_fmt = Format.formatter_of_out_channel ml_out
  and c_fmt = Format.formatter_of_out_channel c_out in
  Format.fprintf c_fmt "%s@\n" c_headers;
  Cstubs.write_c c_fmt ~prefix:"gccjit_stub_" (module B);
  Cstubs.write_ml ml_fmt ~prefix:"gccjit_stub_" (module B);
  Format.pp_print_flush ml_fmt ();
  Format.pp_print_flush c_fmt ();
  close_out ml_out;
  close_out c_out

let () = main ()
