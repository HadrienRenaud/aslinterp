(********************************************************************************************)
(* Operations *)

open Syntax
open Values
open Errors

let eval_binop v1 o v2 =
  match (v1, o, v2) with
  | Bitstr s1, Eq, Bitstr s2 -> Ok (Bool (Array.for_all2 (==) s1 s2))
  | Bitstr s1, NEq, Bitstr s2 -> Ok (Bool (Array.exists2 (!=) s1 s2))
  | _, Eq, _ -> Ok (Bool (v1 == v2))
  | _, NEq, _ ->
      Ok (Bool (v1 != v2)) (* It should be working on all kinds of immutable values *)
  (* Operations on Int *)
  | Int x, LT, Int y -> Ok (Bool (x < y))
  | Int x, Leq, Int y -> Ok (Bool (x <= y))
  | Int x, GT, Int y -> Ok (Bool (x > y))
  | Int x, Geq, Int y -> Ok (Bool (x >= y))
  | Int x, Plus, Int y -> Ok (Int Z.(x + y))
  | Int x, Minus, Int y -> Ok (Int Z.(x - y))
  | Int x, Mult, Int y -> Ok (Int Z.(x * y))
  | Int _, DIV, Int y when y = Z.zero -> Error DivisionByZero
  | Int x, DIV, Int y -> Ok (Int Z.(x / y))
  | Int _, MOD, Int y when y = Z.zero -> Error DivisionByZero
  | Int x, MOD, Int y -> Ok (Int Z.(x mod y))
  | Int x, RSh, Int y -> Ok (Int Z.(x asr to_int y))
  | Int x, LSh, Int y -> Ok (Int Z.(x lsl to_int y))
  | Int x, Pow, Int y -> Ok (Int Z.(x ** to_int y))
  (* Operations on Real *)
  | Real x, LT, Real y -> Ok (Bool (x < y))
  | Real x, Leq, Real y -> Ok (Bool (x <= y))
  | Real x, GT, Real y -> Ok (Bool (x > y))
  | Real x, Geq, Real y -> Ok (Bool (x >= y))
  | Real x, Plus, Real y -> Ok (Real Q.(x + y))
  | Real x, Minus, Real y -> Ok (Real Q.(x - y))
  | Real x, Mult, Real y -> Ok (Real Q.(x * y))
  | Real _, RDiv, Real y when y = Q.zero -> Error DivisionByZero
  | Real x, RDiv, Real y -> Ok (Real Q.(x / y))
  | Real x, Pow, Int y -> Ok (Real (Q.of_float (Q.to_float x ** Z.to_float y)))
  (* Operations on boolean *)
  | Bool a, BAnd, Bool b -> Ok (Bool (a && b))
  | Bool a, BOr, Bool b -> Ok (Bool (a || b))
  | Bool a, BEq, Bool b -> Ok (Bool (a == b))
  | Bool a, BImpl, Bool b -> Ok (Bool ((not a) || b))
  (* Operations on bitstrings *)
  (* Here we assume that length s1 == length s2 *)
  | Bitstr s1, EOR, Bitstr s2 -> Ok (Bitstr (Array.map2 ( != ) s1 s2))
  | Bitstr s1, And, Bitstr s2 -> Ok (Bitstr (Array.map2 ( && ) s1 s2))
  | Bitstr s1, Or, Bitstr s2 -> Ok (Bitstr (Array.map2 ( || ) s1 s2))
  | Bitstr s1, Plus, Bitstr s2 ->
      let i1 = z_of_bitstring s1 in
      let i2 = z_of_bitstring s2 in
      let s3 = bitstring_of_z (Z.add i1 i2) (Array.length s1) in
      Ok (Bitstr s3)
  | Bitstr s1, Minus, Bitstr s2 ->
      let i1 = z_of_bitstring s1 in
      let i2 = z_of_bitstring s2 in
      let s3 = bitstring_of_z (Z.sub i1 i2) (Array.length s1) in
      Ok (Bitstr s3)
  | Bitstr s1, Plus, Int i2 ->
      let i1 = z_of_bitstring s1 in
      let s3 = bitstring_of_z (Z.add i1 i2) (Array.length s1) in
      Ok (Bitstr s3)
  | Bitstr s1, Minus, Int i2 ->
      let i1 = z_of_bitstring s1 in
      let s3 = bitstring_of_z (Z.sub i1 i2) (Array.length s1) in
      Ok (Bitstr s3)
  | _ ->
      Error
        (UnsupportedOperation
           (Format.asprintf "Unsupported operation %a for operands@ %a@ and %a"
              pp_print_binop o pp_print_value v1 pp_print_value v2))

let eval_unop o v =
  match (o, v) with
  | UBNeg, Bool b -> Ok (Bool (not b))
  | UNot, Bitstr s -> Ok (Bitstr (Array.map not s))
  | UMinus, Int x -> Ok (Int (Z.neg x))
  | UMinus, Real x -> Ok (Real (Q.neg x))
  | _ ->
      Error
        (UnsupportedOperation
           (Format.asprintf "Unsupported operation %a for operand@ %a"
              pp_print_unop o pp_print_value v))

(********************************************************************************************)
(* Interpretor Functor *)

module type INTERPRETOR = sig
  type context

  val do_one_step_expr :
    context -> Syntax.expr -> (context * Syntax.expr) Errors.result

  val eval_expr :
    context -> Syntax.expr -> (context * Values.value) Errors.result

  val do_one_step_stmt :
    context -> Syntax.stmt -> (context * Syntax.stmt) Errors.result

  val eval_stmt : context -> Syntax.stmt -> context Errors.result
end

module type SEMANTIC_DESCRIPTOR = sig
  val semi_column_concurrent : bool
end

module SequentialSemantics = struct
  let semi_column_concurrent = false
end

module MakeInterpretor (Ctx : Context.CONTEXT) (SD : SEMANTIC_DESCRIPTOR) =
struct
  (********************************************************************************************)
  (* Expressions *)
  type context = Ctx.t

  open Syntax

  let ( let* ) = Result.bind

  let rec is_map_literal = function
    | EVar _ -> true
    | EMapAccess (e1, ELiteral _) -> is_map_literal e1
    | _ -> false

  let rec exp_to_little_endian = function
    | EVar x -> Ok (x, [])
    | EMapAccess (e1, ELiteral vi) ->
        let* i = value_to_index vi in
        let* x, rev_addr_e1 = exp_to_little_endian e1 in
        Ok (x, i :: rev_addr_e1)
    | _ ->
        Error
          (InterpretorError
             "Cannot transform expr into address: Not fully reduced.")

  let exp_to_big_endian e =
    let* x, rev_addr = exp_to_little_endian e in
    Ok (x, List.rev rev_addr)

  let rec do_one_step_expr c e =
    match e with
    (* Rule Extract-Context  *)
    | EVar x ->
        let* v = Ctx.find x [] c in
        Ok (c, ELiteral v)
    (* Rule Reduce-Unop *)
    | EUnop (o, ELiteral v) ->
        let* v' = eval_unop o v in
        Ok (c, ELiteral v')
    (* Rule Reduce-Binop *)
    | EBinop (ELiteral v1, o, ELiteral v2) ->
        let* v = eval_binop v1 o v2 in
        Ok (c, ELiteral v)
    (* Rule Reduce-Map-Access *)
    | _ when is_map_literal e ->
        let* x, addr = exp_to_big_endian e in
        let* v' = Ctx.find x addr c in
        Ok (c, ELiteral v')
    (* Rule Reduce-Map-Access-Literal *)
    | EMapAccess (ELiteral va, ELiteral vi) ->
        let* i = value_to_index vi in
        let* v' = find_address_in_value va [ i ] in
        Ok (c, ELiteral v')
    (*******************)
    (* Rule Progress-Unop *)
    | EUnop (o, e') ->
        let* c, e'' = do_one_step_expr c e' in
        Ok (c, EUnop (o, e''))
    (* Rule Progress-Binop-Right *)
    | EBinop (e1, o, e2) when not (is_literal e2) ->
        let* c, e2' = do_one_step_expr c e2 in
        Ok (c, EBinop (e1, o, e2'))
    (* Rule Progress-Binop-Left *)
    | EBinop (e1, o, e2) when not (is_literal e1) ->
        let* c, e1' = do_one_step_expr c e1 in
        Ok (c, EBinop (e1', o, e2))
    (* Rule Progress-Map-Access-left *)
    | EMapAccess (e1, e2) when (not (is_map_literal e1)) && not (is_literal e1)
      ->
        let* c, e1' = do_one_step_expr c e1 in
        Ok (c, EMapAccess (e1', e2))
    (* Rule Progress-Map-Access-Right *)
    | EMapAccess (e1, e2) when not (is_literal e2) ->
        let* c, e2' = do_one_step_expr c e2 in
        Ok (c, EMapAccess (e1, e2'))
    (* Final match to guard everything.
       Should not happen, but otherwise this does not compile. *)
    | _ -> Error BlockedInterpretor

  let rec eval_expr c e =
    match do_one_step_expr c e with
    | Ok (c', ELiteral v) -> Ok (c', v)
    | Ok (c', e') -> eval_expr c' e'
    | Error err -> Error err

  (********************************************************************************************)
  (* Statements *)

  let rec is_lexpr_literal = function
    | LEVar _ -> true
    | LEMapWrite (le, e) -> is_literal e && is_lexpr_literal le

  let rec do_one_step_lexpr c le =
    match le with
    | LEMapWrite (le, e) when not (is_literal e) ->
        let* c', e' = do_one_step_expr c e in
        Ok (c', LEMapWrite (le, e'))
    | LEMapWrite (le, e) when not (is_lexpr_literal le) ->
        let* c', le' = do_one_step_lexpr c le in
        Ok (c', LEMapWrite (le', e))
    | _ -> Error BlockedInterpretor

  let rec lexpr_to_little_endian = function
    | LEVar x -> Ok (x, [])
    | LEMapWrite (le, ELiteral vi) ->
        let* i = value_to_index vi in
        let* x, rev_addr_le = lexpr_to_little_endian le in
        Ok (x, i :: rev_addr_le)
    | LEMapWrite (_, _) ->
        Error
          (InterpretorError
             "Cannot convert left-hand expression to address: not fully \
              reduced.")

  let lexpr_to_big_endian le =
    let* x, rev_addr = lexpr_to_little_endian le in
    Ok (x, List.rev rev_addr)

  let rec do_one_step_stmt c s =
    match s with
    (* Rule Reduce-Then-Left *)
    | SThen (SPass, s2) -> Ok (c, s2)
    (* Rule Progress-Then-Left *)
    | SThen (s1, s2) ->
        if SD.semi_column_concurrent then raise (Failure "Not yet implemented")
        else
          let* c', s1' = do_one_step_stmt c s1 in
          Ok (c', SThen (s1', s2))
    (* Rule Reduce-Map-Write *)
    | SAssign (le, ELiteral v2) when is_lexpr_literal le ->
        let* x, addr = lexpr_to_big_endian le in
        let* c' = Ctx.set x addr v2 c in
        Ok (c', SPass)
    (* Rule Progress-Assign-Right *)
    | SAssign (le, e) when not (is_literal e) ->
        let* c', e' = do_one_step_expr c e in
        Ok (c', SAssign (le, e'))
    (* Rule Progress-Assign-Left *)
    | SAssign (le, e) when not (is_lexpr_literal le) ->
        let* c', le' = do_one_step_lexpr c le in
        Ok (c', SAssign (le', e))
    (* Rule Reduce-Cond *)
    | SCond (ELiteral (Bool b), s1, s2) -> Ok (c, if b then s1 else s2)
    (* Rule Progress-Cond *)
    | SCond (e, s1, s2) ->
        let* c', e' = do_one_step_expr c e in
        Ok (c', SCond (e', s1, s2))
    | _ -> Error BlockedInterpretor

  let rec eval_stmt c s =
    match do_one_step_stmt c s with
    | Ok (c', SPass) -> Ok c'
    | Ok (c', s') -> eval_stmt c' s'
    | Error e -> Error e
end

module SequentialInterpretor =
  MakeInterpretor
    (Context.Logger (Context.SequentialContext)) (SequentialSemantics)
