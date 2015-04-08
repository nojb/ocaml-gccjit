exception Error of string

type context
type result
type loc
type lvalue
type rvalue
type field
type typ
type structure
type param
type fn
type block

type unary_op =
  | Negate
  | Bitwise_negate
  | Logical_negate

type binary_op =
  | Plus
  | Minus
  | Mult
  | Divide
  | Modulo
  | Bitwise_and
  | Bitwise_xor
  | Bitwise_or
  | Logical_and
  | Logical_or

type comparison =
  | Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge

type function_kind =
  | Exported
  | Internal
  | Imported
  | Always_inline

type _ option =
  | Progname : string option
  | Optimization_level : int option
  | Debuginfo : bool option
  | Dump_initial_tree : bool option
  | Dump_initial_gimple : bool option
  | Dump_generated_code : bool option
  | Dump_summary : bool option
  | Dump_everything : bool option
  | Selfcheck_gc : bool option
  | Keep_intermediaries : bool option

type output_kind =
  | Assembler
  | Object_file
  | Dynamic_library
  | Executable

val acquire : unit -> context
val release : context -> unit
val dump_to_file : context -> ?update_locs:bool -> string -> unit
val get_first_error : context -> string option
val new_location : context -> string -> int -> int -> loc
val new_global : ?loc:loc -> context -> typ -> string -> lvalue
val new_array_type : ?loc:loc -> context -> typ -> int -> typ
val new_field : ?loc:loc -> context -> typ -> field
val new_struct : ?loc:loc -> context -> field list -> structure
val new_union : ?loc:loc -> context -> field list -> typ
val new_function_ptr_type : ?loc:loc -> context -> ?variadic:bool -> typ list -> typ -> typ
val new_param : ?loc:loc -> context -> string -> typ -> param
val new_function : ?loc:loc -> context -> ?variadic:bool -> function_kind -> string -> param list -> typ -> fn
val get_builtin_function : context -> string -> fn
val zero : context -> typ -> rvalue
val one : context -> typ -> rvalue
val new_rvalue_from_double : context -> typ -> float -> rvalue
val new_rvalue_from_int : context -> typ -> int -> rvalue
val new_rvalue_from_ptr : context -> typ -> int -> rvalue
val null : context -> typ -> rvalue
val new_string_literal : context -> string -> rvalue
val new_unary_op : ?loc:loc -> context -> unary_op -> typ -> rvalue -> rvalue
val new_binary_op : ?loc:loc -> context -> binary_op -> typ -> rvalue -> rvalue -> rvalue
val new_comparison : ?loc:loc -> context -> comparison -> rvalue -> rvalue -> rvalue
val new_child_context : context -> context
val new_cast : ?loc:loc -> context -> rvalue -> typ -> rvalue
val new_array_access : ?loc:loc -> rvalue -> rvalue -> lvalue
val new_call : ?loc:loc -> context -> fn -> rvalue list -> rvalue
val new_call_through_ptr : ?loc:loc -> context -> rvalue -> rvalue list -> rvalue
val get_int_type : context -> ?signed:bool -> int -> typ
val dump_reproducer_to_file : context -> string -> unit
val set_logfile : context -> out_channel -> unit
val set_option : context -> 'a option -> 'a -> unit
val compile_to_file : context -> output_kind -> string -> unit
val compile : context -> result

val set_fields : ?loc:loc -> structure -> field list -> unit

val get_code : result -> string -> ('a -> 'b) Ctypes.fn -> 'a -> 'b

val pointer : typ -> typ
val const : typ -> typ
val volatile : typ -> typ

val dereference_field : ?loc:loc -> rvalue -> field -> rvalue
val dereference : ?loc:loc -> rvalue -> rvalue
val get_type : rvalue -> typ

val get_address : ?loc:loc -> lvalue -> rvalue

val new_local : ?loc:loc -> fn -> typ -> string -> lvalue
val new_block : fn -> string -> block
val get_param : fn -> int -> param
val dump_to_dot : fn -> string -> unit

val add_eval : ?loc:loc -> block -> rvalue -> unit
val add_assignment : ?loc:loc -> block -> lvalue -> rvalue -> unit
val add_assignment_op : ?loc:loc -> block -> lvalue -> binary_op -> rvalue -> unit
val add_comment : ?loc:loc -> block -> string -> unit
val get_function : block -> fn
