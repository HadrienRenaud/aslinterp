type err =
  | UnknownIdentifier of string
  | TypeError of string
  | InterpreterError of string

let pp_err chan = function
  | UnknownIdentifier x -> Printf.fprintf chan "Unknown identifier %s" x
  | TypeError msg -> Printf.fprintf chan "Type error: %s" msg
  | InterpreterError msg -> Printf.fprintf chan "Interpreter error: %s" msg

module NativeBackend = struct
  type vint = int
  type vbool = bool
  type vbitvector = int
  type 'a m = unit -> ('a, err) result
  type loc = string
  type value = (vint, vbool, vbitvector) AST.value

  let vint_of_int i = i

  let bind (vm : 'a m) (f : 'a -> 'b m) : 'b m =
   fun () -> Result.bind (vm ()) (fun v -> f v ())

  let prod (r1 : 'a m) (r2 : 'b m) : ('a * 'b) m =
   fun () ->
    match (r1 (), r2 ()) with
    | Error e, _ | _, Error e -> Error e
    | Ok v1, Ok v2 -> Ok (v1, v2)

  let return : 'a -> 'a m = fun v () -> Result.ok v
  let fail : err -> 'a m = fun e () -> Result.error e
  let bind_data = bind
  let bind_seq = bind

  let choice (c : value m) (m_true : 'b m) (m_false : 'b m) : 'b m =
    bind c (function
      | AST.VBool true -> m_true
      | AST.VBool false -> m_false
      | _ -> fail (TypeError "Boolean expected."))

  let failwith msg = fail (InterpreterError msg)

  let binop op v1 v2 =
    let open AST in
    let vint v = return (VInt v) in
    let vbool v = return (VBool v) in
    match (op, v1, v2) with
    (* int -> int -> int *)
    | PLUS, VInt v1, VInt v2 -> vint (v1 + v2)
    | MUL, VInt v1, VInt v2 -> vint (v1 * v2)
    | MINUS, VInt v1, VInt v2 -> vint (v1 - v2)
    | DIV, VInt v1, VInt v2 -> vint (v1 / v2)
    (* int -> int -> bool*)
    | EQ_OP, VInt v1, VInt v2 -> vbool (v1 == v2)
    | NEQ, VInt v1, VInt v2 -> vbool (v1 <> v2)
    | LEQ, VInt v1, VInt v2 -> vbool (v1 <= v2)
    | LT, VInt v1, VInt v2 -> vbool (v1 < v2)
    | GEQ, VInt v1, VInt v2 -> vbool (v1 >= v2)
    | GT, VInt v1, VInt v2 -> vbool (v1 > v2)
    (* bool -> bool -> bool *)
    | BAND, VBool b1, VBool b2 -> vbool (b1 && b2)
    | BOR, VBool b1, VBool b2 -> vbool (b1 || b2)
    | _ -> assert false

  let unop op v =
    match (op, v) with
    | AST.NEG, AST.VInt i -> return (AST.VInt ~-i)
    | _ -> assert false

  let write_identifier_genv genv x v = return (genv := AST.IMap.add x v !genv)

  let read_identifier_genv genv x _is_data =
    match AST.IMap.find_opt x !genv with
    | Some v -> return v
    | None -> fail (UnknownIdentifier x)

  let write_identifier, read_identifier =
    let genv = ref AST.IMap.empty in
    (write_identifier_genv genv, read_identifier_genv genv)
end

module NativeInterpreter = Interpreter.Make (NativeBackend)

let of_parsed_ast =
  AST.Parsed.tr
    (fun i -> AST.VInt i)
    (fun b -> AST.VBool b)
    (fun s -> VBitVector (int_of_string s))
