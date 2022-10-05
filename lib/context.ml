module S = Syntax
module V = Values
open Errors

module type CONTEXT = sig
  type t

  val empty : t
  val find : S.identifier -> t -> Values.value result
  val set : S.identifier -> Values.value -> t -> t result
  val pp_print : Format.formatter -> t -> unit
end

module SequentialContext : CONTEXT = struct
  type t = V.value S.IdMap.t

  let empty = S.IdMap.empty

  let find x c =
    S.IdMap.find_opt x c |> Option.to_result ~none:(UndefinedVariable x)

  let set x v c = Ok (S.IdMap.add x v c)

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

module Logger (Ctx : CONTEXT) = struct
  include Ctx

  let find x c =
    Format.eprintf "Using %s@\n" x;
    Ctx.find x c

  let set x v c =
    Format.eprintf "Setting %s@\n" x;
    Ctx.set x v c
end
