open Aslinterp.Values
open Aslinterp.Syntax
open Aslinterp.Sem
open Aslinterp.Context
open Aslinterp.Errors
module C = SequentialContext
module S = SequentialInterpretor

let run_and_print = function
  | s, c ->
      Format.printf "-------------------------@\n";
      pp_print_stmt Format.std_formatter s;
      Format.printf "@\n-------------------------@\n";
      (match S.eval_stmt c s with
      | Ok c ->
          Format.printf "OK@\n";
          C.pp_print Format.std_formatter c
      | Error e -> pp_print_error Format.std_formatter e);
      Format.printf "@\n-------------------------\n\n@\n";
      Format.eprintf "@\n"

let () =
  let s =
    stmt_from_list
      [
        SAssign (LEVar "x", ELiteral (make_int 3));
        SAssign (LEVar "y", ELiteral (make_int 5));
        SCond
          ( EBinop (EVar "x", Eq, EVar "y"),
            SAssign (LEVar "r", EBinop (EVar "x", Plus, EVar "y")),
            SAssign (LEVar "r", EBinop (EVar "x", Minus, EVar "y")) );
      ]
  in
  run_and_print (s, C.empty)
