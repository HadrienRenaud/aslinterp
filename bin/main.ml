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
  let s =
    stmt_from_list
      [
        SAssign (LEVar "x", ELiteral (Int 4));
        SAssign (LEVar "y", ELiteral (Int 5));
        SAssign (LEVar "r", EBinop (EVar "x", Plus, EVar "y"));
      ]
  in
  match eval_stmt ctx_empty s with
  | Ok c -> pp_print_context Format.std_formatter c
  | Error e -> pp_print_error Format.std_formatter e

let () = Format.print_newline ()
