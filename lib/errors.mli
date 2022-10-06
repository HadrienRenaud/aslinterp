type error =
  | DivisionByZero
  | TypeError of string
  | UnsupportedOperation of string
  | SemanticError of string
  | UndefinedVariable of string
  | IndexOutOfBounds of string
  | InterpreterError of string
  | BlockedInterpreter

val pp_print_error : Format.formatter -> error -> unit

type 'a result = ('a, error) Result.t
