type unop = BNOT | NEG | NOT

type binop =
  | AND
  | BAND
  | BEQ
  | BOR
  | DIV
  | EOR
  | EQ_OP
  | GT
  | GEQ
  | IMPL
  | LT
  | LEQ
  | MOD
  | MINUS
  | MUL
  | NEQ
  | OR
  | PLUS
  | RDIV
  | SHL
  | SHR

type identifier = string

module ISet = Set.Make (String)
module IMap = Map.Make (String)

type ('i, 'b, 'r, 'bv) value =
  | VInt of 'i
  | VBool of 'b
  | VReal of 'r
  | VBitVector of 'bv
  | VTuple of ('i, 'b, 'r, 'bv) value list
  | VRecord of ('i, 'b, 'r, 'bv) value IMap.t

let value_of_vint i = VInt i
let value_of_vbool b = VBool b
let value_of_vreal r = VReal r
let value_of_vbitvector bv = VBitVector bv

type 'v expr =
  | ELiteral of 'v
  | EVar of identifier
  | EBinop of binop * 'v expr * 'v expr
  | EUnop of unop * 'v expr
  | ECall of identifier * 'v expr list
  | ECond of 'v expr * 'v expr * 'v expr

type 'v lexpr = LEVar of identifier

type 'v stmt =
  | SPass
  | SThen of 'v stmt * 'v stmt
  | SAssign of 'v lexpr * 'v expr
  | SCall of identifier * 'v expr list
  | SReturn of 'v expr list
  | SCond of 'v expr * 'v stmt * 'v stmt

type 'v func = identifier * identifier list * 'v stmt

type 'v decl =
  | Func of 'v func
  | Enum of string list
  | GlobalConst of identifier * 'v expr

type 'v t = 'v decl list

let rec stmt_from_list = function
  | [] -> SPass
  | [ x ] -> x
  | h :: t -> SThen (h, stmt_from_list t)

let use_expr include_funcs : 'v expr -> ISet.t =
  let rec use_ acc = function
    | ELiteral _ -> acc
    | EVar x -> ISet.add x acc
    | EBinop (_op, e1, e2) -> use_ (use_ acc e2) e1
    | EUnop (_op, e) -> use_ acc e
    | ECall (x, args) ->
        let acc = if include_funcs then ISet.add x acc else acc in
        List.fold_left use_ acc args
    | ECond (e1, e2, e3) -> use_ (use_ (use_ acc e1) e3) e2
  in
  use_ ISet.empty

let tr_values tr_int tr_bool tr_real tr_bitvector =
  let rec value_ = function
    | VInt i -> tr_int i
    | VBool b -> tr_bool b
    | VBitVector s -> tr_bitvector s
    | VReal r -> tr_real r
    | VRecord r -> VRecord (IMap.map value_ r)
    | VTuple li -> VTuple (List.map value_ li)
  and expr_ = function
    | ELiteral v -> ELiteral (value_ v)
    | EVar x -> EVar x
    | EUnop (op, e) -> EUnop (op, expr_ e)
    | EBinop (op, e1, e2) -> EBinop (op, expr_ e1, expr_ e2)
    | ECond (e1, e2, e3) -> ECond (expr_ e1, expr_ e2, expr_ e3)
    | ECall (x, es) -> ECall (x, List.map expr_ es)
  and lexpr_ = function LEVar x -> LEVar x
  and stmt_ = function
    | SPass -> SPass
    | SAssign (le, e) -> SAssign (lexpr_ le, expr_ e)
    | SCond (e, s1, s2) -> SCond (expr_ e, stmt_ s1, stmt_ s2)
    | SCall (x, es) -> SCall (x, List.map expr_ es)
    | SReturn es -> SReturn (List.map expr_ es)
    | SThen (s1, s2) -> SThen (stmt_ s1, stmt_ s2)
  and decl_ = function
    | Func (x, args, body) -> Func (x, args, stmt_ body)
    | Enum s -> Enum s
    | GlobalConst (x, e) -> GlobalConst (x, expr_ e)
  in

  List.map decl_

type parsed_value = (int, bool, float, string) value
type parsed_t = parsed_value t
