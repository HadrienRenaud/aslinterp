type err =
  | UnknownIdentifier of string
  | TypeError of string
  | InterpreterError of string

val pp_err : out_channel -> err -> unit

module NativeBackend :
  Backend.S
    with type vint = int
     and type vbool = bool
     and type vbitvector = int
     and type 'a m = unit -> ('a, err) result
     and type loc = string

module NativeInterpreter : Interpreter.S with module B = NativeBackend

val of_parsed_ast : AST.parsed_t -> NativeBackend.value AST.t
