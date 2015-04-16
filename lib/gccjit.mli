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

(** Ocaml bindings for [libgccjit].

    See {{:https://gcc.gnu.org/wiki/JIT}GCC wiki page} for more information. *)

exception Error of string
(** This exception (containing an explanatory string) is raised if an error
    occurs. *)

type context
(** A {!context} encapsulates the state of a compilation.  You can set up
    options on it (see {!set_option}), and add {{!types}types},
    {{!functions}functions} and {{!code}code}, using the API below.

    Invoking {!compile} on it gives you a {!result}, representing in-memory
    machine-code.

    You can call {!compile} repeatedly on one context, giving multiple
    independent results.

    Similarly, you can call {!compile_to_file} on a context to compile to disk.

    Eventually you can call {!release_context} to clean up the context; any
    in-memory results created from it are still usable. *)

type result
(** A {!result} encapsulates the result of an in-memory compilation. *)

type location
(** A {!location} encapsulates a source code location, so that you can
    (optionally) associate locations in your languages with statements in the
    JIT-compiled code, alowing the debugger to single-step through your
    language.

    See {{!locations}Source locations}. *)

type param

type lvalue
(** A [lvalue] is a storage location within your code (e.g. a variable, a
    parameter, etc).  It is also a [rvalue]. *)

type rvalue
(** A [rvalue] is an expression within your code, with some type. *)

type field
(** A [field] encapsulates a field within a struct; it is used when creating a
    struct type (using {!new_struct_type}).  Fields cannot be shared between
    structs. *)

type struct_ = [ `Struct of Gccjit_bindings.gcc_jit_struct ]
(** A [structure] encapsulates a struct type, either one that we have the layout
    for, or an opaque type. *)

type type_ = [ `Type of Gccjit_bindings.gcc_jit_type | struct_ ]
(** A [typ] encapsulates a type e.g. [int] or a [struct foo*]. *)

type function_
(** A [function_] encapsulates a function: either one that you're creating yourself, or
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
    Negate
  | Bitwise_negate
  | Logical_negate

type binary_op =
    Plus
  | Minus
  | Mult
  | Divide
  | Modulo
  | Bitwise_and
  | Bitwise_xor
  | Bitwise_or
  | Logical_and
  | Logical_or

type comparison = Eq | Ne | Lt | Le | Gt | Ge

(** Kinds of function.  *)
type function_kind =
    FUNCTION_Exported
  (** Function is defined by the client code and visible by name outside of the
      JIT. *)

  | FUNCTION_Internal
  (** Function is defined by the client code, but is invisible outside of the
      JIT.  Analogous to a ["static"] function. *)

  | FUNCTION_Imported
  (** Function is not defined by the client code; we're merely referring to it.
       Analogous to using an ["extern"] function from a header file. *)

  | FUNCTION_Always_inline
  (** Function is only ever inlined into other functions, and is invisible
      outside of the JIT.  Analogous to prefixing with ["inline"] and adding
      [__attribute__((always_inline))].  Inlining will only occur when the
      optimization level is above 0; when optimization is off, this is
      essentially the same as {!Internal}. *)

type global_kind =
    GLOBAL_Exported
  | GLOBAL_Internal
  | GLOBAL_Imported

(** Context options.  Set with {!set_option}. *)
type _ context_option =
    OPTION_Progname : string context_option
  (** The name of the program, for used as a prefix when printing error messages
      to stderr.  If not set, ["libgccjit.so"] is used. *)

  | OPTION_Optimization_level : int context_option
  (** How much to optimize the code.  Valid values are [0-3], corresponding to
      GCC's command-line options -O0 through -O3.

      The default value is 0 (unoptimized). *)

  | OPTION_Debuginfo : bool context_option
  (** If [true], {!compile} will attempt to do the right thing so that if you
      attach a debugger to the process, it will be able to inspect variables and
      step through your code.  Note that you can't step through code unless you
      set up source location information for the code (by creating and passing
      in {!location} instances).  *)

  | OPTION_Dump_initial_tree : bool context_option
  (** If [true], {!compile} will dump its initial "tree" representation of
      your code to [stderr] (before any optimizations).  *)

  | OPTION_Dump_initial_gimple : bool context_option
  (** If [true], {!compile} will dump the "gimple" representation of your
      code to stderr, before any optimizations are performed.  The dump resembles
      C code.  *)

  | OPTION_Dump_generated_code : bool context_option
  (** If [true], {!compile} will dump the final generated code to stderr,
      in the form of assembly language.  *)

  | OPTION_Dump_summary : bool context_option
  (** If [true], {!compile} will print information to stderr on the
      actions it is performing, followed by a profile showing the time taken and
      memory usage of each phase. *)

  | OPTION_Dump_everything : bool context_option
  (** If [true], {!compile} will dump copious amount of information on
      what it's doing to various files within a temporary directory.  Use
      {!Keep_intermediates} (see below) to see the results.  The files are
      intended to be human-readable, but the exact files and their formats are
      subject to change. *)

  | OPTION_Selfcheck_gc : bool context_option
  (** If [true], [libgccjit] will aggressively run its garbage collector,
      to shake out bugs (greatly slowing down the compile).  This is likely to
      only be of interest to developers *of* the library.  It is used when
      running the selftest suite.  *)

  | OPTION_Keep_intermediates : bool context_option
  (** If [true], {!release_context} will not clean up intermediate files written
       to the filesystem, and will display their location on stderr.  *)

(** Kinds of ahead-of-time compilation, for use with {!compile_to_file}.  *)
type output_kind =
    OUTPUT_Assembler
  (** Compile the context to an assembler file. *)

  | OUTPUT_Object_file
  (** Compile the context to an object file. *)

  | OUTPUT_Dynamic_library
  (** Compile the context to a dynamic library. *)

  | OUTPUT_Executable
  (** Compile the context to an executable. *)

type type_kind =
    TYPE_Void
  | TYPE_Void_ptr
  | TYPE_Bool
  | TYPE_Char
  | TYPE_Signed_char
  | TYPE_Unsigned_char
  | TYPE_Short
  | TYPE_Unsigned_short
  | TYPE_Int
  | TYPE_Unsigned_int
  | TYPE_Long
  | TYPE_Unsigned_long
  | TYPE_Long_long
  | TYPE_Unsigned_long_long
  | TYPE_Float
  | TYPE_Double
  | TYPE_Long_double
  | TYPE_Const_char_ptr
  | TYPE_Size_t
  | TYPE_File_ptr
  | TYPE_Complex_float
  | TYPE_Complex_double
  | TYPE_Complex_long_double

module Context : sig
  (** {1 Compilation contexts} *)

  (** {2 Lifetime-management} *)

  val acquire : unit -> context
  (** This function acquires a new {!context} instance, which is independent of
      any others that may be present within this process. *)

  val release : context -> unit
  (** This function releases all resources associated with the given context.
      Both the context itsel and all of its object instances are claed up.  It
      should be called exactly once on a given context.

      It is invalid to use the context or any of its {e contextual} objects
      after calling this. *)

  val child_context : context -> context
  (** Given an existing JIT context, create a child context.

      The child inherits a copy of all option-settings from the parent.

      The child can reference objects created within the parent, but not vice-versa.

      The lifetime of the child context must be bounded by that of the parent: you
      should release a child context before releasing the parent context.

      If you use a function from a parent context within a child context, you have
      to compile the parent context before you can compile the child context, and
      the {!result} of the parent context must outlive the {!result} of the
      child context.

      This allows caching of shared initializations. For example, you could create
      types and declarations of global functions in a parent context once within a
      process, and then create child contexts whenever a function or loop becomes
      hot. Each such child context can be used for JIT-compiling just one function
      or loop, but can reference types and helper functions created within the
      parent context.

      Contexts can be arbitrarily nested, provided the above rules are followed,
      but it's probably not worth going above 2 or 3 levels, and there will likely
      be a performance hit for such nesting. *)

  (** {2 Thread-safety}

      Instances of {!context} created via {!acquire_context} are independent from each
      other: only one thread may use a given context at once, but multiple threads
      could each have their own contexts without needing locks.

      Contexts created via {!new_child_context} are related to their parent
      context. They can be partitioned by their ultimate ancestor into independent
      "family trees". Only one thread within a process may use a given "family
      tree" of such contexts at once, and if you're using multiple threads you
      should provide your own locking around entire such context partitions. *)

  (** {2 Debugging} *)

  val dump_to_file : context -> ?update_locs:bool -> string -> unit
  (** To help with debugging: dump a C-like representation to the given path,
      describing what's been set up on the context.  If [~update_locs] true, then
      also set up {!location} information throughout the context, pointing at the
      dump file as if it were a source file.  This may be of use in conjunction
      with {!Debuginfo} to allow stepping through the code in a debugger. *)

  val set_logfile : context -> Unix.file_descr option -> unit
  (** To help with debugging; enable ongoing logging of the context's activity to
      the given file descriptor.

      {[
        set_logfile ctx logfile
      ]}

      Examples of information logged include:

      - API calls
      - the various steps involved within compilation
      - activity on any {!result} instances created by the context
      - activity within any child contexts
      - An example of a log can be seen here, though the precise format and kinds of
        information logged is subject to change.

      The caller remains responsible for closing [logfile], and it must not be
      closed until all users are released. In particular, note that child
      {{!context}contexts} and {!result} instances created by the {!context} will
      use the logfile.

      There may a performance cost for logging.

      You can turn off logging on [ctx] by passing [None] for [logfile]. Doing so
      only affects the context; it does not affect child {{!context}contexts} or
      {!result} instances already created by the {!context}. *)

  val dump_reproducer_to_file : context -> string -> unit
  (** Write C source code into path that can be compiled into a self-contained
      executable (i.e. with [libgccjit] as the only dependency). The generated
      code will attempt to replay the API calls that have been made into the given
      context.

      This may be useful when debugging the library or client code, for reducing a
      complicated recipe for reproducing a bug into a simpler form. For example,
      consider client code that parses some source file into some internal
      representation, and then walks this IR, calling into [libgccjit]. If this
      encounters a bug, a call to {!dump_reproducer_to_file} will write out C code
      for a much simpler executable that performs the equivalent calls into
      [libgccjit], without needing the client code and its data.

      Typically you need to supply [-Wno-unused-variable] when compiling the
      generated file (since the result of each API call is assigned to a unique
      variable within the generated C source, and not all are necessarily then
      used). *)

  (* val get_debug_string : [< object_] -> string *)
  (** Get a human-readable description of this object.

      For example,

      {[
        Printf.printf "obj: %s\n" (get_debug_string obj)
      ]}

      might give this text on [stdout]:

      {[
        obj: 4.0 * (float)i
      ]} *)

  (** {2 Options} *)

  val set_option : context -> 'a context_option -> 'a -> unit
  (** Set an option of the {!context}. *)

  (** {1 Compiling a context}

    Once populated, a {!context} can be compiled to machine code, either
    in-memory via {!compile} or to disk via {!compile_to_file}.

    You can compile a context multiple times (using either form of compilation),
    although any errors that occur on the context will prevent any future
    compilation of that context. *)

  val compile : context -> result
  (** This calls into GCC and builds the code, returning a {!result}.  See
      {{!inmemory}In-memory compilation}. *)

  (** {2 Ahead-of-time compilation}

      Although [libgccjit] is primarily aimed at just-in-time compilation, it can
      also be used for implementing more traditional ahead-of-time compilers, via
      the {!compile_to_file} API entrypoint. *)

  val compile_to_file : context -> output_kind -> string -> unit
  (** Compile the context to a file of the given kind.  This can be called more
      that once on a given context, although any errors that occur will block
      further compilation. *)
end

module Type : sig
  (** {1:types Types} *)

  (** {2 Standard types} *)

  val standard : context -> type_kind -> type_
  (** Access a specific type.  See {!type_kind}. *)

  val int_gen : context -> ?signed:bool -> int -> type_
  (** Get the integer type of the given size and signedness. *)

  val int : context -> type_
  (** Standard [int] type. *)

  (** {2 Pointers, const, and volatile} *)

  val pointer : [< type_] -> type_
  (** Given type [T], get type [T*] *)

  val const : [< type_] -> type_
  (** Given type [T], get type [const T]. *)

  val volatile : [< type_] -> type_
  (** Given type [T], get type [volatile T]. *)

  val array : ?loc:location -> context -> [< type_] -> int -> type_
  (** Given type [T], get type [T[N]] (for a constant [N]). *)

  val function_ptr : ?loc:location -> context -> ?variadic:bool -> type_ list -> type_ -> type_

  (** {2 Structures and unions}

      You can model C struct types by creating [struct_] and [field] instances, in
      either order:

      - by creating the fields, then the structure. For example, to model:

        {[
          struct coord {double x; double y; };
        ]}

        you could call:

        {[
          let field_x = new_field ctx double_type "x" in
          let field_y = new_field ctx double_type "y" in
          let coord = new_struct_type ctx "coord" [ field_x ; field_y ]
        ]}

      - by creating the structure, then populating it with fields, typically to
        allow modelling self-referential structs such as:

        {[
          struct node { int m_hash; struct node *m_next; };
        ]}

        like this:

        {[
          let node = new_opaque_struct ctx "node" in
          let node_ptr = get_pointer node in
          let field_hash = new_field ctx int_type "m_hash" in
          let field_next = new_field ctx node_ptr "m_next" in
          set_fields node [ field_hash; field_next ]
        ]} *)

  val field : ?loc:location -> context -> [< type_] -> string -> field
  (** Create a field, with the given type and name. *)

  val struct_ : ?loc:location -> context -> string -> field list -> struct_
  (** Create a struct type, with the given name and fields. *)

  val opaque_struct : ?loc:location -> context -> string -> struct_
  (** Construct a new struct type, with the given name, but without specifying the
      fields. The fields can be omitted (in which case the size of the struct is not
      known), or later specified using {!set_fields}. *)

  val set_fields : ?loc:location -> struct_ -> field list -> unit
  (** Populate the fields of a formerly-opaque struct type.

      This can only be called once on a given struct type. *)

  val union : ?loc:location -> context -> string -> field list -> type_
  (** Unions work similarly to structs. *)
end

(** {1 Expressions} *)

module RValue : sig
  (** {2:rvalues Rvalues}

      A {!rvalue} is an expression that can be computed.

      It can be simple, e.g.:

      - an integer value e.g. [0] or [42]
      - a string literal e.g. ["Hello world"]
      - a variable e.g. i. These are also {{!lvalues}lvalues} (see below).

      or compound e.g.:

      - a unary expression e.g. [!cond]
      - a binary expression e.g. [(a + b)]
      - a function call e.g. [get_distance (&player_ship, &target)]
      - etc.

      Every {!rvalue} has an associated {{!type_}type}, and the API will check to
      ensure that types match up correctly (otherwise the context will emit an
      error). *)

  val type_of : rvalue -> type_
  (** Get the type of this {!rvalue}. *)

  (** {2 Simple expressions} *)

  val int : context -> type_ -> int -> rvalue
  (** Given a numeric type (integer or floating point), build an {!rvalue} for the
      given constant int value. *)

  val zero : context -> type_ -> rvalue
  (** Given a numeric type (integer or floating point), get the {!rvalue} for
      zero. Essentially this is just a shortcut for:

      {[
        new_rvalue_from_int ctx numeric_type 0
      ]} *)

  val one : context -> type_ -> rvalue
  (** Given a numeric type (integer or floating point), get the {!rvalue} for
      one. Essentially this is just a shortcut for:

      {[
        new_rvalue_from_int ctx numeric_type 1
      ]} *)

  val double : context -> type_ -> float -> rvalue
  (** Given a numeric type (integer or floating point), build an {!rvalue} for the
      given constant double value. *)

  val ptr : context -> type_ -> 'a Ctypes.ptr -> rvalue
  (** Given a pointer type, build an {!rvalue} for the given address. *)

  val null : context -> type_ -> rvalue
  (** Given a pointer type, build an {!rvalue} for [NULL]. Essentially this is
      just a shortcut for:

      {[
        new_rvalue_from_ptr ctx pointer_type Ctypes.null
      ]} *)

  val string_literal : context -> string -> rvalue
  (** Generate an {!rvalue} for the given [NIL]-terminated string, of type
      [Const_char_ptr]. *)

  (** {2 Unary operations} *)

  val unary_op : ?loc:location -> context -> unary_op -> type_ -> rvalue -> rvalue
  (** Build a unary operation out of an input {!rvalue}.  See {!unary_op}. *)

  (** {2 Binary operations} *)

  val binary_op : ?loc:location -> context -> binary_op -> type_ -> rvalue -> rvalue -> rvalue
  (** Build a binary operation out of two constituent {{!rvalue}rvalues}. See
      {!binary_op}. *)

  (** {2 Comparisons} *)

  val comparison : ?loc:location -> context -> comparison -> rvalue -> rvalue -> rvalue
  (** Build a boolean {!rvalue} out of the comparison of two other
      {{!rvalue}rvalues}. *)

  (** {2 Function calls} *)

  val call : ?loc:location -> context -> function_ -> rvalue list -> rvalue
  (** Given a function and the given table of argument rvalues, construct a call
      to the function, with the result as an {!rvalue}.

      {3 Note}

      [new_call] merely builds a [rvalue] i.e. an expression that can be
      evaluated, perhaps as part of a more complicated expression.  The call won't
      happen unless you add a statement to a function that evaluates the expression.

      For example, if you want to call a function and discard the result (or to call a
      function with [void] return type), use [add_eval]:

      {[
        (* Add "(void)printf (args);". *)
        add_eval block (new_call ctx printf_func args)
      ]} *)

  val indirect_call : ?loc:location -> context -> rvalue -> rvalue list -> rvalue
  (** Call through a function pointer. *)

  (** {2 Type-coercion} *)

  val cast : ?loc:location -> context -> rvalue -> type_ -> rvalue
  (** Given an {!rvalue} of [T], construct another {!rvalue} of another type.

      Currently only a limited set of conversions are possible:

      - [int <-> float]
      - [int <-> bool]
      - [P* <-> Q*], for pointer types [P] and [Q] *)

  val lvalue : lvalue -> rvalue

  val param : param -> rvalue
end

module LValue : sig
  (** {2:lvalues Lvalues}

      An {!lvalue} is something that can of the left-hand side of an assignment: a
      storage area (such as a variable). It is also usable as an {!rvalue}, where
      the {!rvalue} is computed by reading from the storage area. *)

  val address : ?loc:location -> lvalue -> rvalue
  (** Taking the address of an {!lvalue}; analogous to [&(EXPR)] in C. *)

  (** {2 Global variables} *)

  val global : ?loc:location -> context -> global_kind -> [< type_] -> string -> lvalue
  (** Add a new global variable of the given type and name to the context.

      The {!global_kind} parameter determines the visibility of the {e global}
      outside of the {!result}. *)

  (** {2 Working with pointers, structs, and unions} *)

  val deref : ?loc:location -> rvalue -> lvalue
  (** Dereferencing a pointer; analogous to [*(EXPR)] in C. *)

  val deref_field : ?loc:location -> rvalue -> field -> lvalue
  (** Accessing a field of an [rvalue] of pointer type, analogous [(EXPR)->field]
      in C, itself equivalent to [(\*EXPR).FIELD] *)

  val array_access : ?loc:location -> rvalue -> rvalue -> lvalue
  (** Given an rvalue of pointer type [T *], get at the element [T] at the given
      index, using standard C array indexing rules i.e. each increment of index
      corresponds to [sizeof(T)] bytes. Analogous to [PTR[INDEX]] in C (or,
      indeed, to [PTR + INDEX]). *)

  val param : param -> lvalue
end

(** {1:functions Creating and using functions} *)

module Param : sig
  (** {2 Params}

      A {!param} represents a parameter to a function. *)

  val create : ?loc:location -> context -> [< type_] -> string -> param
  (** In preparation for creating a function, create a new parameter of the given
      type and name. *)
end

module Function : sig
  (** {2 Functions} *)

  val create :
    ?loc:location -> context -> ?variadic:bool -> function_kind -> [< type_] -> string -> param list -> function_
  (** Create a gcc_jit_function with the given name and parameters.  See
      {!function_kind}. *)

  val builtin : context -> string -> function_
  (** Create a reference to a builtin function (sometimes called intrinsic
      functions). *)

  val param : function_ -> int -> param
  (** Get a specific param of a function by index (0-based). *)

  val dump_dot : function_ -> string -> unit
  (** Emit the function in graphviz format to the given path. *)

  val local : ?loc:location -> function_ -> [< type_] -> string -> lvalue
  (** Add a new local variable within the function, of the given type and name. *)
end

module Block : sig
  (** {2 Blocks}

      A {!block} represents a basic block within a function i.e. a sequence of
      statements with a single entry point and a single exit point.

      The first basic block that you create within a function will be the entrypoint.

      Each basic block that you create within a function must be terminated,
      either with a conditional, a jump, or a return.

      It's legal to have multiple basic blocks that return within one function. *)

  val create : ?name:string -> function_ -> block
  (** Create a block.  You can give it a meaningful name, which may show up in
      dumps of the internal representation, and in error messages. *)

  val parent : block -> function_
  (** Which function is this block within? *)

  (** {2:code Statements} *)

  val eval : ?loc:location -> block -> rvalue -> unit
  (** Add evaluation of an {!rvalue}, discarding the result (e.g. a function call
      that {e returns} void).  This is equivalent to this C code:

      {[
        (void)expression;
      ]} *)

  val assign : ?loc:location -> block -> lvalue -> rvalue -> unit
  (** Add evaluation of an {!rvalue}, assigning the result to the given {!lvalue}.
      This is roughly equivalent to this C code:

      {[
        lvalue = rvalue;
      ]} *)

  val assign_op : ?loc:location -> block -> lvalue -> binary_op -> rvalue -> unit
  (** Add evaluation of an rvalue, using the result to modify an lvalue.  This
      is analogous to ["+="] and friends:

      {[
        lvalue += rvalue;
        lvalue *= rvalue;
        lvalue /= rvalue;
        etc
      ]}

      For example:

      {[
        (* "i++" *)
        add_assignment_op loop_body i Plus (one ctx int_type)
      ]} *)

  val comment : ?loc:location -> block -> string -> unit
  (** Add a no-op textual comment to the internal representation of the code.
      It will be optimized away, but will be visible in the dumps seen via
      {!Dump_initial_tree} and {!Dump_initial_gimple} and thus may be of use when
      debugging how your project's internal representation gets converted to the
      [libgccjit] IR.  *)

  val cond_jump : ?loc:location -> block -> rvalue -> block -> block -> unit
  (** Terminate a block by adding evaluation of an rvalue, branching on the
      result to the appropriate successor block.  This is roughly equivalent to
      this C code:

      {[
        if (boolval)
          goto on_true;
        else
          goto on_false;
      ]} *)

  val jump : ?loc:location -> block -> block -> unit
  (** Terminate a block by adding a jump to the given target block.  This is
      roughly equivalent to this C code:

      {[
        goto target;
      ]} *)

  val return : ?loc:location -> block -> rvalue -> unit
  (** Terminate a block by adding evaluation of an {!rvalue}, returning the
      value.  This is roughly equivalent to this C code:

      {[
        return expression;
      ]} *)

  val return_void : ?loc:location -> block -> unit
  (** Terminate a block by adding a valueless return, for use within a function
      with [void] return type.  This is equivalent to this C code:

      {[
        return;
      ]} *)
end

module Location : sig
  (** {1:locations Source Locations}

      A {!location} encapsulates a source code location, so that you can (optionally)
      associate locations in your language with statements in the JIT-compiled code,
      allowing the debugger to single-step through your language.

      {!location} instances are optional: you can always omit them to any API
      entrypoint accepting one.

      You can construct them using {!new_location}.

      You need to enable [Debuginfo] on the {!context} for these locations to
      actually be usable by the debugger:

      {[
        set_option ctx Debuginfo true
      ]}

      {3 Faking it}

      If you don't have source code for your internal representation, but need to
      debug, you can generate a C-like representation of the functions in your
      context using {!dump_to_file}:

      {[
        dump_to_file ctx ~update_locs:true "/tmp/something.c"
      ]}

      This will dump C-like code to the given path. If the update_locations
      argument is true, this will also set up {!location} information throughout
      the context, pointing at the dump file as if it were a source file, giving
      you something you can step through in the debugger. *)

  val create : context -> string -> int -> int -> location
  (** Create a {!location} instance representing the given source location. *)
end

module Result : sig
  (** {2:inmemory In-memory compilation} *)

  val code : result -> string -> ('a -> 'b) Ctypes.fn -> 'a -> 'b
  (** Locate a given function within the built machine code.

      Functions are looked up by name. For this to succeed, a function with a name
      matching funcname must have been created on result's context (or a parent
      context) via a call to {!new_function} with kind [Exported]:

      {[
        new_function ctx Exported any_return_type funcname (* etc. *)
      ]}

      If such a function is not found, an error will be raised.

      If the function is found, the result is cast to the given Ctypes signature.
      Care must be taken to pass a signature compatible with that of function
      being extracted.

      Note that the resulting machine code becomes invalid after {!release_result}
      is called on the {!result}; attempting to call it after that may lead to a
      segmentation fault. *)

  val global : result -> string -> 'a Ctypes.typ -> 'a Ctypes.ptr
  (** Locate a given global within the built machine code.

      Globals are looked up by name. For this to succeed, a global with a name
      matching name must have been created on result's context (or a parent context)
      via a call to {!new_global} with kind [Global_exported].

      If the global is found, the result is cast to the Given [Ctypes] type.

      This is a pointer to the global, so e.g. for an [int] this is an [int *].

      For example, given an [int foo;] created this way:

      {[
        let exported_global = new_global ctx Global_exported int_type "foo"
      ]}

      we can access it like this:

      {[
        let ptr_to_foo = get_global result "foo" Ctypes.int
      ]}

      If such a global is not found, an error will be raised.

      Note that the resulting address becomes invalid after {!release_result}
      is called on the {!result}; attempting to use it after that may lead to a
      segmentation fault.  *)

  val release : result -> unit
  (** Once we're done with the code, this unloads the built [.so] file. This
      cleans up the result; after calling this, it’s no longer valid to use the
      result, or any code or globals that were obtained by calling {!get_code} or
      {!get_global} on it. *)
end
