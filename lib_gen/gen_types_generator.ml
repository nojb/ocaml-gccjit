let c_headers = "#include <libgccjit.h>"

let main () =
  Format.fprintf Format.std_formatter "%s@\n" c_headers;
  Cstubs_structs.write_c Format.std_formatter (module Gccjit_bindings.Enums);
  Format.pp_print_flush Format.std_formatter ()

let () = main ()
