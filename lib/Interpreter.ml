module type S = sig
  module B : Backend.S

  type ast = B.value AST.t

  val run : ast -> B.value list -> B.value list B.m
end

module Make (B : Backend.S) = struct
  module B = B
  open B

  type genv = value AST.IMap.t
  type ast = value AST.t

  let ( let* ) = B.bind_data
  let ( and* ) = B.prod

  let prod_map f =
    let one acc elt =
      let* v = f elt and* li = acc in
      return (v :: li)
    in
    List.fold_left one (return [])

  let value_of_int i : value = AST.VInt (vint_of_int i)

  let build_enums (ast : ast) : genv =
    let build_one (counter, genv) name =
      let genv = AST.IMap.add name (value_of_int counter) genv in
      (counter + 1, genv)
    in
    let build_decl acc = function
      | AST.Enum ids -> List.fold_left build_one acc ids
      | _ -> acc
    in
    let _, genv = List.fold_left build_decl (0, AST.IMap.empty) ast in
    genv

  (* build every constant and make an global env *)
  let build_consts (ast : ast) : genv -> genv m =
    let rec eval_one acc name =
      match AST.IMap.find_opt name acc with
      | Some (Either.Left v) -> return (v, acc)
      | Some (Either.Right e) ->
          let* v, acc = eval_expr acc e in
          return (v, AST.IMap.add name (Either.Left v) acc)
      | _ -> assert false
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
      | ECall _ -> assert false
      | ECond _ -> assert false
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
    let collect acc genv =
      let* acc = acc in
      let acc_items = AST.IMap.to_seq acc in
      let one_item = function
        | name, Either.Left v -> (name, v)
        | _ -> assert false
      in
      let new_items = Seq.map one_item acc_items in
      let genv = AST.IMap.add_seq new_items genv in
      return genv
    in
    fun genv -> collect (eval_all (return init_acc)) genv

  type eval_res = Returning of value list | Continuing

  let rec eval_expr genv ast is_data =
    let open AST in
    function
    | ELiteral v -> return v
    | EVar x when IMap.mem x genv -> return (IMap.find x genv)
    | EVar x -> read_identifier x is_data
    | EBinop (op, e1, e2) ->
        let* v1 = eval_expr genv ast is_data e1
        and* v2 = eval_expr genv ast is_data e2 in
        binop op v1 v2
    | EUnop (op, e) ->
        let* v = eval_expr genv ast is_data e in
        unop op v
    | ECond (e1, e2, e3) ->
        let eval_ = eval_expr genv ast is_data in
        choice (eval_ e1) (eval_ e2) (eval_ e3)
    | ECall (name, args) -> (
        let* vargs = prod_map (eval_expr genv ast is_data) args in
        let* returned = eval_func genv ast name vargs in
        match returned with
        | [ v ] -> return v
        | _ ->
            failwith (Printf.sprintf "Return arrity error for function %s" name)
        )

  and eval_stmt genv ast =
    let open AST in
    function
    | SPass -> return Continuing
    | SAssign (LEVar x, e) ->
        let* v = eval_expr genv ast true e in
        let* () = write_identifier x v in
        return Continuing
    | SReturn es ->
        let* vs = prod_map (eval_expr genv ast true) es in
        return (Returning vs)
    | SThen (s1, s2) -> (
        let* r1 = eval_stmt genv ast s1 in
        match r1 with
        | Continuing -> eval_stmt genv ast s2
        | Returning vs -> return (Returning vs))
    | SCall (name, args) ->
        let* vargs = prod_map (eval_expr genv ast true) args in
        let* _ = eval_func genv ast name vargs in
        return Continuing
    | SCond (e, s1, s2) ->
        choice
          (eval_expr genv ast true e)
          (eval_stmt genv ast s1) (eval_stmt genv ast s2)

  and eval_func genv ast name args =
    let arg_names, body =
      let has_name = function
        | AST.Func (x, _, _) -> String.equal x name
        | _ -> false
      in
      match List.find_opt has_name ast with
      | Some (AST.Func (_x, arg_names, body)) -> (arg_names, body)
      | _ -> assert false
    in
    let one_arg x v = AST.(SAssign (LEVar x, ELiteral v)) in
    let body =
      AST.SThen (AST.stmt_from_list (List.map2 one_arg arg_names args), body)
    in
    let* res = eval_stmt genv ast body in
    match res with Continuing -> return [] | Returning vs -> return vs

  let run (ast : ast) (main_args : value list) : value list m =
    let genv = build_enums ast in
    let* genv = build_consts ast genv in
    eval_func genv ast "main" main_args
end
