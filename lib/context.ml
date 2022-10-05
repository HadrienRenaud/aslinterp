module S = Syntax
module V = Values
open Errors

module type CONTEXT = sig
  type t

  val empty : t
  val find : S.identifier -> Values.value list -> t -> Values.value result
  val set : S.identifier -> Values.value list -> Values.value -> t -> t result
  val pp_print : Format.formatter -> t -> unit
end

module SequentialContext : CONTEXT = struct
  type t = V.value S.IdMap.t

  let empty = S.IdMap.empty

  let find x addr c =
    match S.IdMap.find_opt x c with
    | None -> Error (UndefinedVariable x)
    | Some v -> Values.find_in_value v addr

  let set x addr v c =
    match addr with
    | [] -> Ok (S.IdMap.add x v c)
    | _ -> (
        match S.IdMap.find_opt x c with
        | None -> Error (UndefinedVariable x)
        | Some w -> (
            match Values.set_in_value w addr v with
            | Error e -> Error e
            | Ok w' -> Ok (S.IdMap.add x w' c)))

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

  let set x addr v c =
    Format.eprintf "Setting %s@\n" x;
    Ctx.set x addr v c
end
