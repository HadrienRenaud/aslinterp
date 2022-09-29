module S = Syntax
module V = Values
open Errors

module type CONTEXT = sig
  type t

  val empty : t
  val find : S.identifier -> V.address -> t -> Values.value result
  val set : S.identifier -> V.address -> Values.value -> t -> t result
  val pp_print : Format.formatter -> t -> unit
end

module SequentialContext : CONTEXT = struct
  type t = V.value S.IdMap.t

  let empty = S.IdMap.empty

  let find x addr c =
    match S.IdMap.find_opt x c with
    | None -> Error (UndefinedVariable x)
    | Some v -> V.find_address_in_value v addr

  let set x addr v c =
    let ( let* ) = Result.bind in
    match addr with
    | [] -> Ok (S.IdMap.add x v c)
    | _ -> (
        match S.IdMap.find_opt x c with
        | None -> Error (UndefinedVariable x)
        | Some va ->
            let* va' = V.set_address_in_value va addr v in
            Ok (S.IdMap.add x va' c))

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

  let pp_print_maybe_address f = function
    | x, [] -> Format.pp_print_string f x
    | x, addr ->
        Format.fprintf f "@[<2>%s[@,%a@;<0-2>]@]" x V.pp_print_address addr

  let find x addr c =
    Format.eprintf "Using %a@\n" pp_print_maybe_address (x, addr);
    Ctx.find x addr c

  let set x addr v c =
    Format.eprintf "Setting %a@\n" pp_print_maybe_address (x, addr);
    Ctx.set x addr v c
end
