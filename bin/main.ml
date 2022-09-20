open Aslinterp.Values
open Aslinterp.Syntax
open Aslinterp.Sem

let () =
  pp_print_expr Format.std_formatter
    (EBinop
       ( ELiteral (Int 3),
         Plus,
         EBinop (EVar "X", Mult, EUnop (UMinus, ELiteral (Int (-4)))) ))

let () = Format.print_newline ()

let () =
  let r =
    EBinop (ELiteral (Bitstr (bitstring_of_int 16 32)), Minus, ELiteral (Int 4))
  in
  match eval_expr ctx_empty r with
  | Ok (_, v) -> pp_print_value Format.std_formatter v
  | Error e -> pp_print_error Format.std_formatter e

let () = Format.print_newline ()
