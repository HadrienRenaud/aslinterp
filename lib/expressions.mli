(** Expression as defined by ARM ARM, section K16.4.5 *)
type expression =
  | Literal of Values.value
  | Var of string
  | Unknown
  | Slice of expression * expression * expression option
  | BinOp of expression * Operations.operation * expression
  | UnOp of Operations.operation * expression
  | FunCall of string * expression list

val pp_print_expression : Format.formatter -> expression -> unit
