module type S = sig
  module B : Backend.S

  type ast = B.value AST.t
  type sfunc = B.value list -> B.value list B.m

  val run : ast -> (string * sfunc) list -> B.value list -> B.value list B.m
end

module Make (B : Backend.S) = struct
  module B = B
  open B

  type sfunc = value list -> value list m
  type ast = value AST.t

  let ( let* ) = B.bind_data
  let ( and* ) = B.prod

  let prod_map f =
    let one acc elt =
      let* v = f elt and* li = acc in
      return (v :: li)
    in
    fun li ->
      let* li = List.fold_left one (return []) li in
      return (List.rev li)

  let value_of_int i : value = AST.VInt (vint_of_int i)

  (*****************************************************************************)
  (*                                                                           *)
  (*                       Global constants environment                        *)
  (*                                                                           *)
  (*****************************************************************************)

  module GEnv = struct
    include AST.IMap

    type elt =
      | Value of value
      | Func of int ref * value AST.func
      | SpecialFunc of sfunc

    type t = elt AST.IMap.t

    let add_value name v = add name (Value v)
    let add_seq_value s = add_seq (Seq.map (fun (x, v) -> (x, Value v)) s)

    let add_seq_func s =
      add_seq (Seq.map (fun (x, f) -> (x, Func (ref 0, f))) s)

    let add_seq_special_func s =
      add_seq (Seq.map (fun (x, f) -> (x, SpecialFunc f)) s)

    let find_opt_value name env =
      Option.bind (find_opt name env) (function Value v -> Some v | _ -> None)

    let mem_value name env = Option.is_some (find_opt_value name env)
    let find_value name env = Option.get (find_opt_value name env)
  end

  module LEnv = AST.IMap

  type genv = GEnv.t
  type lenv = value LEnv.t
  type env = genv * lenv

  (*****************************************************************************)
  (*                                                                           *)
  (*                      Construction of the initial env                      *)
  (*                                                                           *)
  (*****************************************************************************)

  let build_enums (ast : ast) : genv =
    let build_one (counter, genv) name =
      let genv = GEnv.add_value name (value_of_int counter) genv in
      (counter + 1, genv)
    in
    let build_decl acc = function
      | AST.Enum ids -> List.fold_left build_one acc ids
      | _ -> acc
    in
    let _, genv = List.fold_left build_decl (0, AST.IMap.empty) ast in
    genv

  (* build every constant and make an global env *)
  let build_consts (ast : ast) genv : genv m =
    let rec eval_one acc name =
      match GEnv.find_opt_value name genv with
      | Some v -> return (v, acc)
      | None -> (
          match AST.IMap.find_opt name acc with
          | Some (Either.Left v) -> return (v, acc)
          | Some (Either.Right e) ->
              let* v, acc = eval_expr acc e in
              return (v, AST.IMap.add name (Either.Left v) acc)
          | _ -> failwith ("Unknown constant " ^ name))
    and eval_expr acc e =
      let open AST in
      match e with
      | ELiteral v -> return (v, acc)
      | EVar x -> eval_one acc x
      | EUnop (op, e') ->
          let* v', acc = eval_expr acc e' in
          let* v = B.unop op v' in
          return (v, acc)
      | EBinop (op, e1, e2) ->
          let* v1, acc = eval_expr acc e1 in
          let* v2, acc = eval_expr acc e2 in
          let* v = B.binop op v1 v2 in
          return (v, acc)
      | ECond (e1, e2, e3) ->
          let* v, acc = eval_expr acc e1 in
          choice (return v) (eval_expr acc e2) (eval_expr acc e3)
      | ECall _ ->
          failwith "Function calling in constants is not yet implemented"
    in
    let init_acc =
      let one_decl acc = function
        | AST.GlobalConst (name, e) -> AST.IMap.add name (Either.Right e) acc
        | _ -> acc
      in
      List.fold_left one_decl AST.IMap.empty ast
    in
    let eval_all acc =
      let one_decl acc = function
        | AST.GlobalConst (name, _e) ->
            let* acc = acc in
            let* _, acc = eval_one acc name in
            return acc
        | _ -> acc
      in
      List.fold_left one_decl acc ast
    in
    let collect acc =
      let* acc = acc in
      let acc_items = AST.IMap.to_seq acc in
      let one_item = function
        | name, Either.Left v -> (name, v)
        | _ -> assert false
      in
      let new_items = Seq.map one_item acc_items in
      let genv = GEnv.add_seq_value new_items genv in
      return genv
    in
    collect (eval_all (return init_acc))

  let build_funcs ast genv =
    List.to_seq ast
    |> Seq.filter_map (function
         | AST.Func (name, args, body) -> Some (name, (name, args, body))
         | _ -> None)
    |> fun s -> GEnv.add_seq_func s genv

  type eval_res = Returning of value list | Continuing of lenv

  let continue ((_genv, lenv) : env) = return (Continuing lenv)

  (*****************************************************************************)
  (*                                                                           *)
  (*                       Main interpretation functions                       *)
  (*                                                                           *)
  (*****************************************************************************)

  let rec eval_expr (env : env) scope is_data =
    let genv, lenv = env in
    let open AST in
    function
    | ELiteral v -> return v
    | EVar x when GEnv.mem_value x genv -> return (GEnv.find_value x genv)
    | EVar x when LEnv.mem x lenv ->
        let v = LEnv.find x lenv in
        let* () = on_read_identifier x scope v in
        return v
    | EVar x -> failwith (Printf.sprintf "Unknown identifier: %s" x)
    | EBinop (op, e1, e2) ->
        let* v1 = eval_expr env scope is_data e1
        and* v2 = eval_expr env scope is_data e2 in
        binop op v1 v2
    | EUnop (op, e) ->
        let* v = eval_expr env scope is_data e in
        unop op v
    | ECond (e1, e2, e3) ->
        let eval_ = eval_expr env scope is_data in
        choice (eval_ e1) (eval_ e2) (eval_ e3)
    | ECall (name, args) -> (
        let* vargs = prod_map (eval_expr env scope is_data) args in
        let* returned = eval_func (fst env) name vargs in
        match returned with
        | [ v ] -> return v
        | _ ->
            failwith (Printf.sprintf "Return arrity error for function %s" name)
        )

  and eval_stmt (env : env) scope =
    let open AST in
    function
    | SPass -> continue env
    | SAssign (LEVar x, e) ->
        let* v = eval_expr env scope true e in
        let* () = on_write_identifier x scope v in
        let genv, lenv = env in
        let lenv = LEnv.add x v lenv in
        continue (genv, lenv)
    | SReturn es ->
        let* vs = prod_map (eval_expr env scope true) es in
        return (Returning vs)
    | SThen (s1, s2) ->
        bind_seq (eval_stmt env scope s1) (fun r1 ->
            match r1 with
            | Continuing lenv -> eval_stmt (fst env, lenv) scope s2
            | Returning vs -> return (Returning vs))
    | SCall (name, args) ->
        let* vargs = prod_map (eval_expr env scope true) args in
        let* _ = eval_func (fst env) name vargs in
        continue env
    | SCond (e, s1, s2) ->
        choice
          (eval_expr env scope true e)
          (eval_stmt env scope s1) (eval_stmt env scope s2)

  and eval_func genv name args =
    match GEnv.find_opt name genv with
    | None -> failwith ("Unknown function: " ^ name)
    | Some (GEnv.Value _) -> failwith ("Cannot call value " ^ name)
    | Some (GEnv.SpecialFunc f) -> f args
    | Some (GEnv.Func (r, (_, arg_names, body))) -> (
        let scope = (name, !r) in
        let () = r := !r + 1 in
        let one_arg x v = AST.(SAssign (LEVar x, ELiteral v)) in
        let body =
          let arg_names =
            if List.compare_lengths args arg_names == 0 then arg_names
            else
              List.to_seq arg_names
              |> Seq.take (List.length args)
              |> List.of_seq
          in
          AST.SThen (AST.stmt_from_list (List.map2 one_arg arg_names args), body)
        in
        let* res = eval_stmt (genv, LEnv.empty) scope body in
        match res with Continuing _ -> return [] | Returning vs -> return vs)

  let run (ast : ast) std_lib_extras (main_args : value list) : value list m =
    let genv = build_enums ast in
    let* genv = build_consts ast genv in
    let genv = build_funcs ast genv in
    let genv = GEnv.add_seq_special_func (List.to_seq std_lib_extras) genv in
    eval_func genv "main" main_args
end
