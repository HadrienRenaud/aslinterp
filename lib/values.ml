type bitstring = bool array
type index = IInt of Z.t | IString of string
type address = index list
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

let list_separator f () = F.fprintf f ",@ "

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
        (F.pp_print_list ~pp_sep:list_separator pp_print_map_element)
        l

let pp_print_address = F.pp_print_list ~pp_sep:list_separator pp_print_index

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
let make_array l = Map (List.mapi (fun i x -> (IInt (Z.of_int i), x)) l)

module Addresses = struct
  let ( let* ) = Result.bind

  open Errors
  open List

  let rec find_address_in_value v addr =
    match (v, addr) with
    | _, [] -> Ok v
    | Map a, h :: t when mem_assoc h a -> find_address_in_value (assoc h a) t
    | Map a, h :: _t ->
        Error
          (IndexOutOfBounds
             (Format.asprintf "%a in %a" pp_print_index h pp_print_value (Map a)))
    | _, _h :: _t ->
        Error
          (UnsupportedOperation
             (Format.asprintf "Cannot index %a." pp_print_value v))

  let rec set_address_in_value v addr new_value =
    match (v, addr) with
    | _, [] ->
        Error
          (InterpretorError "Problem setting value. This should not happen.")
    | Map a, h :: [] -> Ok (Map ((h, new_value) :: remove_assoc h a))
    | Map a, h :: t when mem_assoc h a ->
        let* v' = set_address_in_value (assoc h a) t new_value in
        Ok (Map ((h, v') :: remove_assoc h a))
    | Map a, h :: _t ->
        Error
          (IndexOutOfBounds
             (Format.asprintf "%a in %a" pp_print_index h pp_print_value (Map a)))
    | _, _h :: _t ->
        Error
          (UnsupportedOperation
             (Format.asprintf "Cannot index %a." pp_print_value v))
end

let find_address_in_value = Addresses.find_address_in_value
let set_address_in_value = Addresses.set_address_in_value
