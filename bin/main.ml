open Aslinterp.Values
open Aslinterp.Syntax
open Aslinterp.Sem
open Aslinterp.Context
open Aslinterp.Errors

let () =
  let s =
    stmt_from_list
      [
        SAssign (LEVar "x", ELiteral (make_bitstring 5 8));
        SAssign (LEVar "y", ELiteral (make_bitstring 4 8));
        SAssign (LEVar "r", EBinop (EVar "x", Plus, EVar "y"));
        SCond
          ( EBinop (EVar "x", Eq, EVar "y"),
            SAssign (LEVar "r'", EBinop (EVar "x", Plus, ELiteral (make_int 5))),
            SAssign (LEVar "r'", EBinop (EVar "x", Minus, EVar "y")) );
      ]
  in
  pp_print_stmt Format.std_formatter s;
  Format.print_newline ();
  match eval_stmt SequentialContext.empty s with
  | Ok c -> SequentialContext.pp_print Format.std_formatter c
  | Error e -> pp_print_error Format.std_formatter e

let () = Format.print_newline ()
