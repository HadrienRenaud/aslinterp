(* Prolog *)

type error =
  | DivisionByZero
  | UnsupportedOperation of string
  | SemanticError of string
  | UndefinedVariable of string
  | InterpretorError of string
  | BlockedInterpretor
  | OutOfBoundError of int * int

let pp_print_error f e =
  match e with
  | DivisionByZero -> Format.pp_print_string f "Division by zero"
  | UnsupportedOperation s -> Format.fprintf f "Unsupported operation: %s" s
  | SemanticError s -> Format.fprintf f "Semantic error: %s" s
  | UndefinedVariable x -> Format.fprintf f "Variable %s is undefined." x
  | InterpretorError s -> Format.fprintf f "Internal error: %s" s
  | BlockedInterpretor -> Format.pp_print_string f "Blocked interpretor"
  | OutOfBoundError (i, l) ->
      Format.fprintf f "Out of bound array acces: index %d >= length %d" i l

type 'a result = ('a, error) Result.t

(********************************************************************************************)
(* Operations *)

open Syntax
open Values

let eval_binop v1 o v2 =
  match (v1, o, v2) with
  | _, Eq, _ -> Ok (Bool (v1 == v2))
  | _, NEq, _ ->
      Ok (Bool (v1 != v2)) (* It should be working on all kinds of values *)
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
  | Int x, RSh, Int y -> Ok (Int Z.(x asr Z.to_int y))
  | Int x, LSh, Int y -> Ok (Int Z.(x lsl Z.to_int y))
  | Int x, Pow, Int y -> Ok (Int Z.(x ** Z.to_int y))
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
(* Contexts *)

type context = {
  v : Values.value IdMap.t;
  s : Syntax.subpgm IdMap.t;
  d : IdSet.t;
  u : IdSet.t;
}

let ctx_find_opt_var x { v; _ } = IdMap.find_opt x v

let ctx_find_res_var x c =
  ctx_find_opt_var x c |> Option.to_result ~none:(UndefinedVariable x)

let ctx_empty : context =
  { v = IdMap.empty; s = IdMap.empty; d = IdSet.empty; u = IdSet.empty }

let ctx_update_var x v c = { c with v = IdMap.add x v c.v }
let ctx_can_use_var x c = not @@ IdSet.mem x c.d
let ctx_can_set_var x c = (not (IdSet.mem x c.d)) && not (IdSet.mem x c.u)

let rec uses_expr = function
  | ELiteral _ -> IdSet.empty
  | EVar x -> IdSet.singleton x
  | EUnop (_, e) -> uses_expr e
  | EBinop (e1, _, e2) -> IdSet.union (uses_expr e1) (uses_expr e2)
  (* TODO: change next line *)
  | EArrayGet (e1, e2) -> IdSet.union (uses_expr e1) (uses_expr e2)

let defs_expr _ = IdSet.empty
(* For the moment, when we will have function calls, it might get different. *)

let rec uses_stmt = function
  | SPass -> IdSet.empty
  | SAssign (LEVar _, e) -> uses_expr e
  | SThen (s1, s2) -> IdSet.union (uses_stmt s1) (uses_stmt s2)
  | SCond (e, s1, s2) ->
      IdSet.union (uses_expr e) @@ IdSet.union (uses_stmt s1) (uses_stmt s2)

let rec defs_stmt = function
  | SPass -> IdSet.empty
  | SAssign (LEVar x, e) -> IdSet.add x (defs_expr e)
  | SThen (s1, s2) -> IdSet.union (defs_stmt s1) (defs_stmt s2)
  | SCond (e, s1, s2) ->
      IdSet.union (uses_expr e) @@ IdSet.union (defs_stmt s1) (defs_stmt s2)

let ctx_expand_uses_defs s c =
  {
    v = c.v;
    s = c.s;
    u = IdSet.union c.u (uses_stmt s);
    d = IdSet.union c.d (defs_stmt s);
  }

let ctx_discard_uses_defs { d; u; _ } { v; s; _ } = { d; u; v; s }

let pp_print_context f c =
  let pp_print_var f e =
    let x, v = e in
    Format.fprintf f "@[<2>\"%s\":@ %a@]" x pp_print_value v
  in
  Format.fprintf f "@[<hv 2>{ %a@] }"
    (Format.pp_print_seq
       ~pp_sep:(fun f () -> Format.fprintf f ",@ ")
       pp_print_var)
    (IdMap.to_seq c.v)

(********************************************************************************************)
(* Expressions *)

open Syntax

let ( let* ) = Result.bind

let rec do_one_step_expr c e =
  match e with
  (* Rule Extract-Context  *)
  | EVar x when ctx_can_use_var x c ->
      let* v = ctx_find_res_var x c in
      Ok (c, ELiteral v)
  (* Rules Reduce-Unop-* *)
  | EUnop (o, ELiteral v) ->
      let* v' = eval_unop o v in
      Ok (c, ELiteral v')
  (* Rules Progress-Unop-* *)
  | EUnop (o, e') ->
      let* c, e'' = do_one_step_expr c e' in
      Ok (c, EUnop (o, e''))
  (* Rules Reduce-Binop-* *)
  | EBinop (ELiteral v1, o, ELiteral v2) ->
      let* v = eval_binop v1 o v2 in
      Ok (c, ELiteral v)
  (* Rules Progress-Binop-Right-* *)
  | EBinop (e1, o, e2) when not (is_literal e2) ->
      let* c, e2' = do_one_step_expr c e2 in
      Ok (c, EBinop (e1, o, e2'))
  (* Rules Progress-Binop-Left-* *)
  | EBinop (e1, o, e2) when not (is_literal e1) ->
      let* c, e1' = do_one_step_expr c e1 in
      Ok (c, EBinop (e1', o, e2))
  (* Rule Reduce-Array-Get *)
  | EArrayGet (ELiteral (Array a), ELiteral (Int i)) ->
      let i = Z.to_int i in
      if i < Array.length a then Ok (c, ELiteral (Array.get a i))
      else Error (OutOfBoundError (i, Array.length a))
  (* Rule Progress-Array-Right *)
  | EArrayGet (e1, e2) when not (is_literal e2) ->
      let* c, e2' = do_one_step_expr c e2 in
      Ok (c, EArrayGet (e1, e2'))
  (* Rule Progress-Array-Left *)
  | EArrayGet (e1, e2) when not (is_literal e1) ->
      let* c, e1' = do_one_step_expr c e1 in
      Ok (c, EArrayGet (e1', e2))
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

let rec do_one_step_stmt c s =
  match s with
  (* Rule Reduce-Then-Left *)
  | SThen (SPass, s2) -> Ok (c, s2)
  (* Rule Progress-Then-Left *)
  | SThen (s1, s2) ->
      let* c', s1' = do_one_step_stmt c s1 in
      Ok (c', SThen (s1', s2))
  (* Rule Reduce-Assign *)
  | SAssign (LEVar x, ELiteral v) when ctx_can_set_var x c ->
      let c' = ctx_update_var x v c in
      Ok (c', SPass)
  (* Rule Progress-Assign *)
  | SAssign (LEVar x, e) ->
      let* c', e' = do_one_step_expr c e in
      Ok (c', SAssign (LEVar x, e'))
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
