(* The MIT License (MIT)

   Copyright (c) 2015 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

exception Error of string * string

type context
(** A [context] encapsulates the state of a compilation.  You can set up options
    on it (see {!set_option}), and add types (see {!types}), functions (see
    {!functions}) and code, using the API below.

    Invoking {!compile} on it gives you a {!result}, representing in-memory
    machine-code.

    You can call {!compile} repeatedly on one context, giving multiple
    independent results.

    Similarly, you can call {!compile_to_file} on a context to compile to disk.

    Eventually you can call {!release} to clean up the context; any in-memory
    results created from it are still usable. *)

type result
(** A [result] encapsulates the result of an in-memory compilation. *)

type loc
(** A [loc] encapsulates a source code location, so that you can (optionally)
    associate locations in your languages with statements in the JIT-compiled
    code, alowing the debugger to single-step through your language.

    Note that to do so, you need to enable [Debuginfo] on the {!context}. *)

type param'

type lvalue'

type rvalue'

type lvalue = [ `Lvalue of lvalue' | `Param of param' ]
(** A [lvalue] is a storage location within your code (e.g. a variable, a
    parameter, etc).  It is also a [rvalue]. *)

type rvalue = [ `Lvalue of lvalue' | `Rvalue of rvalue' | `Param of param' ]
(** A [rvalue] is an expression within your code, with some type. *)

type field
(** A [field] encapsulates a field within a struct; it is used when creating a
    struct type (using {!new_struct_type}).  Fields cannot be shared between
    structs. *)

type structure'

type typ'

type typ = [ `Struct of structure' | `Type of typ' ]
(** A [typ] encapsulates a type e.g. [int] or a [struct foo*]. *)

type structure = [ `Struct of structure' ]
(** A [structure] encapsulates a struct type, either one that we have the layout
    for, or an opaque type. *)

type param = [ `Param of param' ]

type fn
(** A [fn] encapsulates a function: either one that you're creating yourself, or
    a reference to one that you're dynamically linking to within the ret of the
    process. *)

type block
(** A [block] encapsulates a {e basic block} of statements within a function
    (i.e. with one entry point and one exit point).

    Every block within a function must be terminated with a conditional, a
    branch, or a return.

    The blocks within a function form a directed graph.

    The entrypoint to the function is the first block created within it.

    All of the blocks in a function must be reachable via some path from the
    first block.

    It's OK to have more than one {e return} from a function (i.e., multiple
    blocks that terminate by returning. *)

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

(** Context options.  Set with {!set_option}. *)
type _ option =
  | Progname : string option
  (** The name of the program, for used as a prefix when printing error messages
      to stderr.  If not set, ["libgccjit.so"] is used. *)

  | Optimization_level : int option
  (** How much to optimize the code.  Valid values are [0-3], corresponding to
      GCC's command-line options -O0 through -O3.

      The default value is 0 (unoptimized). *)

  | Debuginfo : bool option
  (** If [true], {!compile} will attempt to do the right thing so that if you
      attach a debugger to the process, it will be able to inspect variables and
      step through your code.  Note that you can't step through code unless you
      set up source location information for the code (by creating and passing
      in {!loc} instances).  *)

  | Dump_initial_tree : bool option
  (** If [true], {!compile} will dump its initial "tree" representation of
      your code to [stderr] (before any optimizations).  *)

  | Dump_initial_gimple : bool option
  (** If [true], {!compile} will dump the "gimple" representation of your
      code to stderr, before any optimizations are performed.  The dump resembles
      C code.  *)

  | Dump_generated_code : bool option
  (** If [true], {!compile} will dump the final generated code to stderr,
      in the form of assembly language.  *)

  | Dump_summary : bool option
  (** If [true], {!compile} will print information to stderr on the
      actions it is performing, followed by a profile showing the time taken and
      memory usage of each phase. *)

  | Dump_everything : bool option
  (** If [true], {!compile} will dump copious amount of information on
      what it's doing to various files within a temporary directory.  Use
      {!Keep_intermediates} (see below) to see the results.  The files are
      intended to be human-readable, but the exact files and their formats are
      subject to change. *)

  | Selfcheck_gc : bool option
  (** If [true], [libgccjit] will aggressively run its garbage collector,
      to shake out bugs (greatly slowing down the compile).  This is likely to
      only be of interest to developers *of* the library.  It is used when
      running the selftest suite.  *)

  | Keep_intermediates : bool option
  (** If [true], {!release} will not clean up intermediate files written to the
       filesystem, and will display their location on stderr.  *)

(** Kinds of ahead-of-time compilation, for use with {!compile_to_file}.  *)
type output_kind =
  | Assembler
  (** Compile the context to an assembler file. *)

  | Object_file
  (** Compile the context to an object file. *)

  | Dynamic_library
  (** Compile the context to a dynamic library. *)

  | Executable
  (** Compile the context to an executable. *)

type type_kind =
  | Void
  | Void_ptr
  | Bool
  | Char
  | Signed_char
  | Unsigned_char
  | Short
  | Unsigned_short
  | Int
  | Unsigned_int
  | Long
  | Unsigned_long
  | Long_long
  | Unsigned_long_long
  | Float
  | Double
  | Long_double
  | Const_char_ptr
  | Size_t
  | File_ptr
  | Complex_float
  | Complex_double
  | Complex_long_double

val acquire : unit -> context
val release : context -> unit

val dump_to_file : context -> ?update_locs:bool -> string -> unit
(** To help with debugging: dump a C-like representation to the given path,
    describing what's been set up on the context.  If [~update_locs] true, then
    also set up {!loc} information throughout the context, pointing at the dump
    file as if it were a source file.  This may be of use in conjunction with
    {!Debuginfo} to allow stepping through the code in a debugger. *)

val new_location : context -> string -> int -> int -> loc
val new_global : ?loc:loc -> context -> [< typ] -> string -> lvalue
val new_array_type : ?loc:loc -> context -> [< typ] -> int -> typ
val new_field : ?loc:loc -> context -> [< typ] -> string -> field
val new_struct : ?loc:loc -> context -> string -> field list -> structure
val new_union : ?loc:loc -> context -> string -> field list -> typ
val new_function_ptr_type : ?loc:loc -> context -> ?variadic:bool -> typ list -> typ -> typ
val new_param : ?loc:loc -> context -> string -> [< typ] -> param
val new_function : ?loc:loc -> context -> ?variadic:bool -> function_kind -> string -> param list -> [< typ] -> fn
val get_builtin_function : context -> string -> fn
val zero : context -> typ -> rvalue
val one : context -> typ -> rvalue
val new_rvalue_from_double : context -> typ -> float -> rvalue
val new_rvalue_from_int : context -> typ -> int -> rvalue
val new_rvalue_from_ptr : context -> typ -> nativeint -> rvalue
val null : context -> typ -> rvalue
val new_string_literal : context -> string -> rvalue
val new_unary_op : ?loc:loc -> context -> unary_op -> typ -> [< rvalue] -> rvalue
val new_binary_op : ?loc:loc -> context -> binary_op -> typ -> [< rvalue] -> [< rvalue] -> rvalue
val new_comparison : ?loc:loc -> context -> comparison -> [< rvalue] -> [< rvalue] -> rvalue
val new_child_context : context -> context
val new_cast : ?loc:loc -> context -> rvalue -> typ -> rvalue
val new_array_access : ?loc:loc -> [< rvalue] -> [< rvalue] -> lvalue
val new_call : ?loc:loc -> context -> fn -> [< rvalue] list -> rvalue
val new_call_through_ptr : ?loc:loc -> context -> [< rvalue] -> [< rvalue] list -> rvalue
val get_int_type : context -> ?signed:bool -> int -> typ
val dump_reproducer_to_file : context -> string -> unit

val set_logfile : context -> Unix.file_descr -> unit
(** To help with debugging; enable ongoing logging of the context's activity to
    the given file descriptor.  The caller remains responsible for closing the
    file descriptor. *)

val set_option : context -> 'a option -> 'a -> unit

val compile_to_file : context -> output_kind -> string -> unit
(** Compile the context to a file of the given kind.  This can be called more
    that once on a given context, although any errors that occur will block
    further compilation. *)

val compile : context -> result

val set_fields : ?loc:loc -> structure -> field list -> unit

val get_code : result -> string -> ('a -> 'b) Ctypes.fn -> 'a -> 'b
(** Locate a given function within the built machine code.  The Ctypes
    signature is used to cast the code so that it can be called.  Care must be
    taken to pass a signature compatible with that of function being extracted. *)

val get_global : result -> string -> 'a Ctypes.typ -> 'a Ctypes.ptr
(** Locate a given global within the built machine code.  It must have been
    created using {!Exported}.  This is a ptr to the global, so e.g. for an
    [int] this is an [int *]. *)

val get_pointer : [< typ] -> typ
val get_const : [< typ] -> typ
val get_volatile : [< typ] -> typ
val get_type : context -> type_kind -> typ

val dereference_field : ?loc:loc -> [< rvalue] -> field -> lvalue
val dereference : ?loc:loc -> [< rvalue] -> lvalue
val type_of : rvalue -> typ

val get_address : ?loc:loc -> [< lvalue] -> rvalue

val new_local : ?loc:loc -> fn -> [< typ] -> string -> lvalue
val new_block : fn -> string -> block
val get_param : fn -> int -> param
val dump_to_dot : fn -> string -> unit

val add_eval : ?loc:loc -> block -> [< rvalue] -> unit
val add_assignment : ?loc:loc -> block -> [< lvalue] -> rvalue -> unit
val add_assignment_op : ?loc:loc -> block -> [< lvalue] -> binary_op -> rvalue -> unit
val add_comment : ?loc:loc -> block -> string -> unit
val end_with_conditional : ?loc:loc -> block -> [< rvalue] -> block -> block -> unit
val end_with_jmp : ?loc:loc -> block -> block -> unit
val end_with_return : ?loc:loc -> block -> [< rvalue] -> unit
val end_with_void_return : ?loc:loc -> block -> unit
val get_function : block -> fn
