type unop = NEG | NOT

type binop =
  | BAND
  | BOR
  | EQ_OP
  | NEQ
  | GT
  | GEQ
  | LT
  | LEQ
  | PLUS
  | MINUS
  | OR
  | AND
  | EOR
  | MUL
  | RDIV
  | DIV
  | SHL
  | SHR

type identifier = string

module ISet : Set.S with type elt = string
module IMap : Map.S with type key = string

type ('i, 'b, 'bv) value =
  | VInt of 'i
  | VBool of 'b
  | VBitVector of 'bv
  | VTuple of ('i, 'b, 'bv) value list
  | VRecord of ('i, 'b, 'bv) value IMap.t

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

val stmt_from_list : 'v stmt list -> 'v stmt
val use_expr : bool -> 'v expr -> ISet.t

module Parsed : sig
  type parsed_value = (int, bool, string) value
  type parsed_t = parsed_value t

  val tr :
    (int -> ('i, 'b, 'bv) value) ->
    (bool -> ('i, 'b, 'bv) value) ->
    (string -> ('i, 'b, 'bv) value) ->
    parsed_t ->
    ('i, 'b, 'bv) value t
end
