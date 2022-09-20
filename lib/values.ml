module Structure = Map.Make (String)

type 'a structure = 'a Structure.t
type bitstring = bool array

type value =
  | Bitstr of bitstring
  | Int of int
  | Real of float
    (* TODO: investigate if there is a possibility for real number with unbounded precision in caml *)
  | Bool of bool
  | Enum of string
    (* as a named value cannot be shared between different enumerations, we do not have to specify of which enumeration we are talking. *)
  | Struct of value structure
  | Tuple of value array
  | Array of value array

module F = Format

let pp_print_bit f b = F.pp_print_int f (if b then 1 else 0)
let make_cutter sep f () = F.fprintf f sep

let rec pp_print_struct_elem f e =
  let k, v = e in
  F.fprintf f "@[<2>\"%s\":@ %a@]" k pp_print_value v

and pp_print_value f v =
  match v with
  | Real x -> F.pp_print_float f x
  | Int x -> F.pp_print_int f x
  | Bool b -> F.pp_print_bool f b
  | Enum s -> F.pp_print_string f s
  | Bitstr a ->
      F.fprintf f "@[<h>\"%a\"@]"
        (F.pp_print_seq ~pp_sep:(make_cutter "") pp_print_bit)
        (Array.to_seq a)
  | Struct s ->
      F.fprintf f "@[<hv 2>{ %a@] }"
        (F.pp_print_seq ~pp_sep:(make_cutter ",@ ") pp_print_struct_elem)
        (Structure.to_seq s)
  | Tuple a ->
      F.fprintf f "@[<hv 2>( %a )@]"
        (F.pp_print_seq ~pp_sep:(make_cutter ",@ ") pp_print_value)
        (Array.to_seq a)
  | Array a ->
      F.fprintf f "@[<hv 2>[ %a ]@]"
        (F.pp_print_seq ~pp_sep:(make_cutter ",@ ") pp_print_value)
        (Array.to_seq a)

let bitstring_of_int n l =
  let s = Array.make l false in
  Array.fold_left_map (fun n _ -> (n / 2, n mod 2 == 1)) n s |> snd

let int_of_bitstring s =
  Array.fold_right (fun b n -> (2 * n) + Bool.to_int b) s 0

let int_pow a n =
  (* Inspired by ocaml batteries
     https://github.com/ocaml-batteries-team/batteries-included/blob/master/src/batNumber.ml#L274 *)
  let rec pow a n =
    if n = 0 then 1
    else if n = 1 then a
    else
      let b = pow a (n / 2) in
      b * b * if n mod 2 == 0 then 1 else a
  in
  pow a n
