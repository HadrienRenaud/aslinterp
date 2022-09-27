module Structure = Map.Make (String)

type 'a structure = 'a Structure.t
type bitstring = bool array

type value =
  | Bitstr of bitstring
  | Int of Z.t
  | Real of Q.t
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
  | Real x -> Q.pp_print f x
  | Int x -> Z.pp_print f x
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

let bitstring_of_z n l =
  let add_trailing_zeros s = Seq.append s (Seq.repeat false) in
  let unfolder n =
    if n == Z.zero then None else Some Z.(n mod ~$2 == one, n / ~$2)
  in
  Seq.unfold unfolder n |> add_trailing_zeros |> Seq.take l |> Array.of_seq

let z_of_bitstring s =
  let folder n i b = if b then Z.(n + (one lsl i)) else n in
  Seq.fold_lefti folder Z.zero (Array.to_seq s)

let make_int x = Int (Z.of_int x)
let make_real x = Real (Q.of_float x)
let make_bitstring x l = Bitstr (bitstring_of_z (Z.of_int x) l)
