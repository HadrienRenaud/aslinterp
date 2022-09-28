(** {1 Tests for the lib/values.ml module} *)

open Aslinterp__Values

(********************************************************************************************)
(** {2 Bitstrings} *)

let zero_bstr = Array.make 8 false

let one_bstr =
  let r = Array.copy zero_bstr in
  Array.set r 0 true;
  r

let two_bstr =
  let r = Array.copy zero_bstr in
  Array.set r 1 true;
  r

(** {3 Deconstruction} *)

let test_deconstruct_one () =
  Alcotest.(check int)
    "good deconstruction of 1" 1
    (Z.to_int (z_of_bitstring one_bstr))

let test_deconstruct_two () =
  Alcotest.(check int)
    "good deconstruction of 2" 2
    (Z.to_int (z_of_bitstring two_bstr))

(** {3 Construction} *)

let test_construct_one () =
  Alcotest.(check (array bool))
    "Good construction of 1" one_bstr
    (bitstring_of_z (Z.of_int 1) 8)

let test_construct_two () =
  Alcotest.(check (array bool))
    "Good construction of 2" two_bstr
    (bitstring_of_z (Z.of_int 2) 8)

(********************************************************************************************)
(** {2 Run tests} *)

let () =
  let open Alcotest in
  run "Values"
    [
      ( "bitstring-construction",
        [
          test_case "one" `Quick test_construct_one;
          test_case "two" `Quick test_construct_two;
        ] );
      ( "bitstring-deconstruction",
        [
          test_case "one" `Quick test_deconstruct_one;
          test_case "two" `Quick test_deconstruct_two;
        ] );
    ]
