open Aslinterp.Values
open Aslinterp.Syntax
open Aslinterp.Sem
open Aslinterp.Context
open Aslinterp.Errors

let () =
  let s =
    stmt_from_list
      [
        SAssign
          ( LEVar "Matrix",
            ELiteral
              (make_array
                 [
                   make_array [ make_int 1; make_int 2; make_int 3 ];
                   make_array [ make_int 4; make_int 5; make_int 6 ];
                   make_array [ make_int 7; make_int 8; make_int 9 ];
                 ]) );
        SAssign (LEVar "zero", ELiteral (make_int 0));
        SAssign (LEVar "one", ELiteral (make_int 1));
        SAssign (LEVar "two", ELiteral (make_int 2));
        SAssign (LEVar "three", ELiteral (make_int 3));
        SAssign (LEVar "four", ELiteral (make_int 4));
        SAssign
          ( LEVar "x",
            EMapAccess (EMapAccess (EVar "Matrix", EVar "one"), EVar "two") );
        SAssign
          ( LEMapWrite (LEMapWrite (LEVar "Matrix", EVar "zero"), EVar "zero"),
            EVar "four" );
        SAssign (LEVar "y", EVar "three");
        SAssign (LEVar "r", EBinop (EVar "x", Plus, EVar "y"));
        SCond
          ( EBinop (EVar "x", Eq, EVar "y"),
            SAssign (LEVar "r'", EBinop (EVar "x", Plus, ELiteral (make_int 5))),
            SAssign (LEVar "r'", EBinop (EVar "x", Minus, EVar "y")) );
      ]
  in
  pp_print_stmt Format.std_formatter s;
  Format.print_newline ();
  match SequentialInterpretor.eval_stmt SequentialContext.empty s with
  | Ok c -> SequentialContext.pp_print Format.std_formatter c
  | Error e -> pp_print_error Format.std_formatter e

let () = Format.print_newline ()
