(** Operators are basic operation identifiers on values *)
type operation =
  | Equal
  | NEqual  (** Test for equality and non equality between 2 values *)
  | LT
  | LEQ
  | GT
  | GEQ
      (** Test for less, les or equal, greater, greater or equal between 2 integers or reals *)
  (* TODO IN *)
  | BNot  (** Boolean inverse *)
  | BAnd
  | BOr  (** Boolean operations and and or *)
  | Not  (** Bitstring obtained by inverting every bit logically *)
  | And
  | Or
  | EOR  (** Operations on every bit of bitstrings *)
  | Slice  (** Operation between a bistring and a tuple *)
  (* TODO put that in clear *)
  | Plus
  | Minus  (** Unary operations on numbers *)
  | Add
  | Sub
  | Mult
  | Div  (** Operations on integers or real *)
  | Mod
  | ScaleUp
  | ScaleDown  (** Operations on intergers *)
  | Pow  (** Power of a number by an integer *)

val pp_print_operation : Format.formatter -> operation -> unit
val arity : operation -> int
val is_unary : operation -> bool
val is_binary : operation -> bool
