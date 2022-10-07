open Aslinterp.Values
open Aslinterp.Syntax
open Aslinterp.Sem
open Aslinterp.Context
open Aslinterp.Errors
module C = SequentialContext
module S = SequentialInterpreter

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

let cas =
  ( (* stmt_from_list
       [
         SAssign (LEVar "comparevalue", EGetArray (EVar "X", EVar "s"));
         SAssign (LEVar "newvalue", EGetArray (EVar "X", EVar "t"));
         SAssign (LEVar "address", EGetArray (EVar "X", EVar "n"));
         SAssign (LEVar "oldvalue", EGetArray (EVar "Mem", EVar "address"));
         SCond
           ( EBinop (EVar "comparevalue", Eq, EVar "oldvalue"),
             SAssign (LESetArray (LEVar "Mem", EVar "address"), EVar "newvalue"),
             SPass );
         SAssign (LESetArray (LEVar "X", EVar "s"), EVar "oldvalue");
       ] *)
    Aslinterp.AslParser.pgm Aslinterp.AslLexer.token
    @@ Lexing.from_string ~with_positions:false
         "comparevalue = X[s];\n\
          newvalue = X[t];\n\
          address = X[n];\n\
          oldvalue = Mem[address];\n\
          if comparevalue == oldvalue then\n\
          Mem[address] = newvalue\n\
          else pass end;\n\
          X[s] = oldvalue",
    let address = Z.of_int 12345 in
    let s = Z.of_int 1 in
    let t = Z.of_int 2 in
    let n = Z.of_int 3 in
    let old_value = make_int 1 in
    let compare_value = make_int 1 in
    let new_value = make_int 2 in
    let memory = VArray [ (address, old_value) ] in
    let x = VArray [ (s, compare_value); (t, new_value); (n, VInt address) ] in
    let c = C.empty in
    let c = Result.get_ok @@ C.set "s" [] (VInt s) c in
    let c = Result.get_ok @@ C.set "t" [] (VInt t) c in
    let c = Result.get_ok @@ C.set "n" [] (VInt n) c in
    let c = Result.get_ok @@ C.set "Mem" [] memory c in
    let c = Result.get_ok @@ C.set "X" [] x c in
    c )

let () = run_and_print cas