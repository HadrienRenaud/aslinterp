module S = Syntax
module V = Values

module type CONTEXT = sig
  type t
  type addr = Syntax.identifier

  val empty : t
  val find : addr -> t -> Values.value
  val set : addr -> Values.value -> t -> t
  val can_use : addr -> t -> bool
  val can_set : addr -> t -> bool
  val pp_print : Format.formatter -> t -> unit
end

module SequentialContext : CONTEXT = struct
  type t = V.value S.IdMap.t
  type addr = Syntax.identifier

  let empty = S.IdMap.empty
  let find x c = S.IdMap.find x c
  let set x v c = S.IdMap.add x v c
  let can_use x c = S.IdMap.mem x c
  let can_set _x _c = true

  let pp_print f c =
    let pp_print_var f e =
      let x, v = e in
      Format.fprintf f "@[<2>\"%s\":@ %a@]" x V.pp_print_value v
    in
    Format.fprintf f "@[<hv 2>{ %a@] }"
      (Format.pp_print_seq
         ~pp_sep:(fun f () -> Format.fprintf f ",@ ")
         pp_print_var)
      (S.IdMap.to_seq c)
end
