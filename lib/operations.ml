type operation =
  | Equal
  | NEqual
  | LT
  | LEQ
  | GT
  | GEQ
  | BNot
  | BAnd
  | BOr
  | Not
  | And
  | Or
  | EOR
  | Slice
  | Plus
  | Minus
  | Add
  | Sub
  | Mult
  | Div
  | Mod
  | ScaleUp
  | ScaleDown
  | Pow

module F = Format

let pp_print_operation f o =
  F.pp_print_string f
    (match o with
    | Equal -> "=="
    | NEqual -> "!="
    | LT -> "<"
    | LEQ -> "<="
    | GT -> ">"
    | GEQ -> ">="
    | BNot -> "!"
    | BAnd -> "&&"
    | BOr -> "||"
    | Not -> "NOT"
    | And -> "AND"
    | Or -> "OR"
    | EOR -> "EOR"
    | Slice -> ""
    | Plus | Add -> "+"
    | Minus | Sub -> "-"
    | Mult -> "*"
    | Div -> "\\"
    | Mod -> "%"
    | ScaleUp -> ">>"
    | ScaleDown -> "<<"
    | Pow -> "^")

let arity o =
  match o with
  | BNot | BAnd | Not | BOr -> 1
  | Equal | NEqual | LT | LEQ | GT | GEQ | And | Or | EOR | Plus | Minus | Add
  | Sub | Mult | Div | Mod | ScaleUp | ScaleDown | Pow ->
      2
  | Slice -> 3

let is_unary o = match o with BNot | BAnd | Not | BOr -> true | _ -> false

let is_binary o =
  match o with
  | Equal | NEqual | LT | LEQ | GT | GEQ | And | Or | EOR | Plus | Minus | Add
  | Sub | Mult | Div | Mod | ScaleUp | ScaleDown | Pow ->
      true
  | _ -> false
