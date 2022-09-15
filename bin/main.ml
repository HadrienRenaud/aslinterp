open Aslinterp.Values
open Aslinterp.Expressions
open Aslinterp.Operations

let () =
  pp_print_expression Format.std_formatter
    (BinOp
       ( Literal (Int 3),
         Add,
         Slice
           ( Var "X",
             UnOp (Plus, Literal (Int (-4))),
             Some (FunCall ("endslice", [ Var "X"; Literal (Enum "Reversed") ]))
           ) ))

let () = Format.print_newline ()
