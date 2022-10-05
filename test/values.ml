(** {1 Tests for the lib/values.ml module} *)

open Aslinterp__Values

(********************************************************************************************)
(** {2 Bitstrings} *)

let one_bstr = [ true; false; false; false; false; false; false; false ]
let two_bstr = [ false; true; false; false; false; false; false; false ]

(** {3 Deconstruction} *)

let test_deconstruct_one () =
  Alcotest.(check int)
    "good deconstruction of 1" 1
    (Z.to_int (z_of_bitvector one_bstr))

let test_deconstruct_two () =
  Alcotest.(check int)
    "good deconstruction of 2" 2
    (Z.to_int (z_of_bitvector two_bstr))

(** {3 Construction} *)

let test_construct_one () =
  Alcotest.(check (list bool))
    "Good construction of 1" one_bstr
    (bitvector_of_z (Z.of_int 1) 8)

let test_construct_two () =
  Alcotest.(check (list bool))
    "Good construction of 2" two_bstr
    (bitvector_of_z (Z.of_int 2) 8)

(********************************************************************************************)
(** {2 Run tests} *)

let () =
  let open Alcotest in
  run "Values"
    [
      ( "bitvector-construction",
        [
          test_case "one" `Quick test_construct_one;
          test_case "two" `Quick test_construct_two;
        ] );
      ( "bitvector-deconstruction",
        [
          test_case "one" `Quick test_deconstruct_one;
          test_case "two" `Quick test_deconstruct_two;
        ] );
    ]
