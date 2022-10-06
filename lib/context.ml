module S = Syntax
module V = Values
open Errors

module type CONTEXT = sig
  type t

  val empty : t
  val find : S.identifier -> V.address -> t -> Values.value result
  val set : S.identifier -> V.address -> Values.value -> t -> t result
  val find_subpgm : Syntax.identifier -> t -> Syntax.subpgm Errors.result
  val pop_locals : t -> t
  val with_globals_from : t -> t -> t
  val pp_print : Format.formatter -> t -> unit

  val of_globals :
    (Syntax.identifier * Values.value) Seq.t -> Syntax.subpgm Seq.t -> t
end

module SequentialContext : CONTEXT = struct
  type t = {
    locals : V.value S.IdMap.t;
    globals : V.value S.IdMap.t;
    subpgms : S.subpgm S.IdMap.t;
  }

  let empty =
    { locals = S.IdMap.empty; globals = S.IdMap.empty; subpgms = S.IdMap.empty }

  let find x addr c =
    let process_value v = V.find_address_in_value v addr in
    match S.IdMap.find_opt x c.locals with
    | Some v -> process_value v
    | None -> (
        match S.IdMap.find_opt x c.globals with
        | Some v -> process_value v
        | None -> Error (UndefinedVariable x))

  let set x addr v c =
    match addr with
    | [] -> Ok { c with locals = S.IdMap.add x v c.locals }
    | _ -> (
        match S.IdMap.find_opt x c.locals with
        | Some w -> (
            match Values.set_address_in_value w addr v with
            | Error e -> Error e
            | Ok w' -> Ok { c with locals = S.IdMap.add x w' c.locals })
        | None -> (
            match S.IdMap.find_opt x c.globals with
            | Some w -> (
                match Values.set_address_in_value w addr v with
                | Error e -> Error e
                | Ok w' -> Ok { c with globals = S.IdMap.add x w' c.globals })
            | None -> Error (UndefinedVariable x)))

  let find_subpgm x c =
    S.IdMap.find_opt x c.subpgms |> Option.to_result ~none:(UndefinedVariable x)

  let pop_locals c = { c with locals = S.IdMap.empty }
  let with_globals_from a b = { a with locals = b.locals }

  let pp_print f c =
    let pp_print_var f e =
      let x, v = e in
      Format.fprintf f "@[<2>\"%s\":@ %a@]" x V.pp_print_value v
    in
    Format.fprintf f "@[<hv 2>{ %a@] }"
      (Format.pp_print_seq
         ~pp_sep:(fun f () -> Format.fprintf f ",@ ")
         pp_print_var)
      (S.IdMap.to_seq c.locals)

  let of_globals globals subpgms =
    let subpgm_mapper p =
      match p with S.Procedure (name, _, _) -> (name, p)
    in
    {
      locals = S.IdMap.empty;
      globals = S.IdMap.of_seq globals;
      subpgms = S.IdMap.of_seq (Seq.map subpgm_mapper subpgms);
    }
end

module Logger (Ctx : CONTEXT) = struct
  include Ctx

  let pp_print_maybe_addr f = function
    | x, [] -> Format.pp_print_string f x
    | x, addr ->
        Format.fprintf f "@[<2>%s@ [ %a ]@]" x
          (Format.pp_print_list
             ~pp_sep:(fun f () -> Format.fprintf f ",@ ")
             Values.pp_print_value)
          addr

  let find x addr c =
    Format.eprintf "Using   %a@\n" pp_print_maybe_addr (x, addr);
    Ctx.find x addr c

  let set x addr v c =
    Format.eprintf "Setting %a@\n" pp_print_maybe_addr (x, addr);
    Ctx.set x addr v c
end
