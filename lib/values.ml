type bitstring = bool array
type index = IInt of Z.t | IString of string
type 'a map = (index * 'a) list

type value =
  | Bitstr of bitstring
  | Int of Z.t
  | Real of Q.t
  | Bool of bool
  | String of string
  | Map of value map

module F = Format

let pp_print_bit f b = F.pp_print_int f (if b then 1 else 0)

let pp_print_index f i =
  match i with
  | IInt z -> Z.pp_print f z
  | IString s -> F.fprintf f "@[<h>'%s'@]" s

let rec pp_print_map_element f = function
  | i, v -> F.fprintf f "@[<h>%a:@ %a@]" pp_print_index i pp_print_value v

and pp_print_value f v =
  match v with
  | Real x -> Q.pp_print f x
  | Int x -> Z.pp_print f x
  | Bool b -> F.pp_print_bool f b
  | String s -> F.fprintf f "@[<h>\"%s\"@]" s
  | Bitstr a ->
      F.fprintf f "@[<h>b\"%a\"@]"
        (F.pp_print_seq ~pp_sep:F.pp_print_cut pp_print_bit)
        (Array.to_seq a)
  | Map l ->
      F.fprintf f "@[<hv 2>[ %a ]@]"
        (F.pp_print_list
           ~pp_sep:(fun f () -> F.fprintf f ", @")
           pp_print_map_element)
        l

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
