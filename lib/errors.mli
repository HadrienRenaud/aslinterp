
type error =
  | DivisionByZero
  | UnsupportedOperation of string
  | SemanticError of string
  | UndefinedVariable of string
  | InterpretorError of string
  | BlockedInterpretor

val pp_print_error : Format.formatter -> error -> unit

type 'a result = ('a, error) Result.t
