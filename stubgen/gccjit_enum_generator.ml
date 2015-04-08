let c_headers = "#include <libgccjit.h>"

let main () =
  let c_out = open_out "gccjit_generated_enums.c" in
  let c_fmt = Format.formatter_of_out_channel c_out in
  Format.fprintf c_fmt "%s@\n" c_headers;
  Cstubs_structs.write_c c_fmt (module Gccjit_stubs.Enums);
  Format.pp_print_flush c_fmt ();
  close_out c_out

let () = main ()
