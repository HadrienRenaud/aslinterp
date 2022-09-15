module F = Format
module O = Operations
module V = Values

type expression =
  | Literal of V.value
  | Var of string
  | Unknown
  | Slice of expression * expression * expression option
  | BinOp of expression * O.operation * expression
  | UnOp of O.operation * expression
  | FunCall of string * expression list

let rec pp_print_expression f e =
  match e with
  | Unknown -> F.pp_print_string f "UNKNOWN"
  | Var x -> F.pp_print_string f x
  | Literal v -> V.pp_print_value f v
  | Slice (a, e1, oe2) ->
      F.fprintf f "@[<h 2>%a@[<h 2>[%a%a]@]@]" pp_print_expression a
        pp_print_expression e1
        (F.pp_print_option (fun f' -> F.fprintf f' ",@ %a" pp_print_expression))
        oe2
  | BinOp (e1, o, e2) ->
      F.fprintf f "@[<2>%a@ %a %a@]" pp_print_expression e1 O.pp_print_operation
        o pp_print_expression e2
  | UnOp (o, e) ->
      F.fprintf f "@[<h>%a%a@]" O.pp_print_operation o pp_print_expression e
  | FunCall (n, l) ->
      F.fprintf f "@[<hv 2>%s(@,%a)@]" n
        (F.pp_print_list
           ~pp_sep:(fun f' () -> F.fprintf f' ",@ ")
           pp_print_expression)
        l
