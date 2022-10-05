type bitvector = bool list

(** Values in CoreASL, ie values from ASL, where compound values are mapped to Map. *)
type value =
  | VBitVec of bitvector  (** Bit strings of fixed size *)
  | VInt of Z.t  (** Integers, unbounded in size, signed *)
  | VReal of Q.t
      (** Real number in the mathematical sense, unbounded in size or precision *)
  | VBool of bool  (** Boolean *)
  | VString of string  (** Strings *)
  | VEnum of string  (** Enums *)
  | VTuple of tuple  (** Tuples **)
  | VRecord of record  (** Record types *)
  | VArray of varray  (** Array *)

and record = (string * value) list
and varray = (Z.t * value) list
and tuple = value list

module F = Format

let pp_print_bit f b = F.pp_print_int f (if b then 1 else 0)
let list_separator f () = F.fprintf f ",@ "

let rec pp_print_array_element f = function
  | i, v -> F.fprintf f "@[<h>%a:@ %a@]" Z.pp_print i pp_print_value v

and pp_print_record_element f = function
  | s, v -> F.fprintf f "@[<h>'%s':@ %a@]" s pp_print_value v

and pp_print_value f v =
  match v with
  | VReal x -> Q.pp_print f x
  | VInt x -> Z.pp_print f x
  | VBool b -> F.pp_print_bool f b
  | VString s -> F.fprintf f "@[<h>\"%s\"@]" s
  | VBitVec l ->
      F.fprintf f "@[<h>b\"%a\"@]"
        (F.pp_print_list ~pp_sep:F.pp_print_cut pp_print_bit)
        l
  | VEnum s -> F.pp_print_string f s
  | VTuple l ->
      F.fprintf f "@[<hv 2>( %a )@]"
        (F.pp_print_list ~pp_sep:list_separator pp_print_value)
        l
  | VRecord l ->
      F.fprintf f "@[<hv 2>{ %a }@]"
        (F.pp_print_list ~pp_sep:list_separator pp_print_record_element)
        l
  | VArray l ->
      F.fprintf f "@[<hv 2>[ %a ]@]"
        (F.pp_print_list ~pp_sep:list_separator pp_print_array_element)
        l

let bitvector_of_z n l =
  let add_trailing_zeros s = Seq.append s (Seq.repeat false) in
  let unfolder n =
    if n == Z.zero then None else Some Z.(n mod ~$2 == one, n / ~$2)
  in
  Seq.unfold unfolder n |> add_trailing_zeros |> Seq.take l |> List.of_seq

let z_of_bitvector s =
  let folder n i b = if b then Z.(n + (one lsl i)) else n in
  Seq.fold_lefti folder Z.zero (List.to_seq s)

let make_int x = VInt (Z.of_int x)
let make_real x = VReal (Q.of_float x)
let make_bitvector x l = VBitVec (bitvector_of_z (Z.of_int x) l)
