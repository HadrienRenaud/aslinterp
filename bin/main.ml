open Aslinterp.Values
open Aslinterp.Syntax
open Aslinterp.Sem
open Aslinterp.Context
open Aslinterp.Errors
module C = SequentialContext
module S = SequentialInterpretor

let cas =
  ( stmt_from_list
      [
        SAssign (LEVar "comparevalue", EMapAccess (EVar "X", EVar "s"));
        SAssign (LEVar "newvalue", EMapAccess (EVar "X", EVar "t"));
        SAssign (LEVar "address", EMapAccess (EVar "X", EVar "n"));
        SAssign (LEVar "oldvalue", EMapAccess (EVar "Mem", EVar "address"));
        SCond
          ( EBinop (EVar "comparevalue", Eq, EVar "oldvalue"),
            SAssign (LEMapWrite (LEVar "Mem", EVar "address"), EVar "newvalue"),
            SPass );
        SAssign (LEMapWrite (LEVar "X", EVar "s"), EVar "oldvalue");
      ],
    let address = Z.of_int 12345 in
    let s = Z.of_int 1 in
    let t = Z.of_int 2 in
    let n = Z.of_int 3 in
    let old_value = make_bitstring 3 8 in
    let compare_value = make_bitstring 3 8 in
    let new_value = make_bitstring 4 8 in
    let memory = Map [ (IInt address, old_value) ] in
    let x =
      Map
        [ (IInt s, compare_value); (IInt t, new_value); (IInt n, Int address) ]
    in
    let c = C.empty in
    let c = Result.get_ok @@ C.set "s" [] (Int s) c in
    let c = Result.get_ok @@ C.set "t" [] (Int t) c in
    let c = Result.get_ok @@ C.set "n" [] (Int n) c in
    let c = Result.get_ok @@ C.set "Mem" [] memory c in
    let c = Result.get_ok @@ C.set "X" [] x c in
    c )

let run_and_print = function
  | s, c ->
      Format.printf "-------------------------@\n";
      pp_print_stmt Format.std_formatter s;
      Format.printf "@\n-------------------------@\n";
      (match S.eval_stmt c s with
      | Ok c -> Format.printf "OK@\n"; C.pp_print Format.std_formatter c
      | Error e -> pp_print_error Format.std_formatter e);
      Format.printf "@\n-------------------------\n\n@\n";
      Format.eprintf "@\n"

let () = run_and_print cas

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
  run_and_print (s, C.empty)
