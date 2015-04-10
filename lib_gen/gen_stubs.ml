let c_headers = "#include <libgccjit.h>"

module B = Gccjit_bindings.Bindings (Gccjit_types_generated)

let prefix = "caml"

let main () =
  let ml_out = open_out "lib/gccjit_stubs_generated.ml"
  and c_out = open_out "lib/gccjit_stubs.c" in
  let ml_fmt = Format.formatter_of_out_channel ml_out
  and c_fmt = Format.formatter_of_out_channel c_out in
  Format.fprintf c_fmt "%s@\n" c_headers;
  Cstubs.write_c c_fmt ~prefix (module B);
  Cstubs.write_ml ml_fmt ~prefix (module B);
  Format.pp_print_flush ml_fmt ();
  Format.pp_print_flush c_fmt ();
  close_out ml_out;
  close_out c_out

let () = main ()
