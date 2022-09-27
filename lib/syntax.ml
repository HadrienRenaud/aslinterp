type binop =
  | BAnd  (** [&&] *)
  | BOr  (** [||] *)
  | BImpl  (** [-->] *)
  | BEq  (** [<->] *)
  | Eq  (** [==] *)
  | NEq  (** [!=] *)
  | GT  (** [>] *)
  | Geq  (** [>=] *)
  | LT  (** [<] *)
  | Leq  (** [<=] *)
  | Plus  (** [+] *)
  | Minus  (** [-] *)
  | Or  (** [OR] *)
  | And  (** [AND] *)
  | EOR  (** [EOR] *)
  | Mult  (** [*] *)
  | RDiv  (** [/] *)
  | DIV  (** [DIV] *)
  | MOD  (** [MOD] *)
  | LSh  (** [<<] *)
  | RSh  (** [>>] *)
  | Pow  (** [^] *)
  | App  (** [++] *)

type unop = UMinus  (** [-] *) | UBNeg  (** [!] *) | UNot  (** [NOT] *)
type identifier = string

module IdMap = Map.Make (String)
module IdSet = Set.Make (String)

type expr =
  | ELiteral of Values.value  (** A direct value *)
  | EVar of identifier  (** A variable *)
  | EUnop of unop * expr  (** [- e] *)
  | EBinop of expr * binop * expr  (** [e1 + e2] *)
(* Unsupported now:
   | EUnknown (** [UNKNWON] *)
   | EUnstable (** [UNSTABLE] *)
   | ECond of expr * expr * expr (** [if e1 then e2 else e3] *)
   | EFunCall of identifier * expr list (** [print(e1, e2)] *)
   | PatternMatching
   | IN (** [3 IN {2, 3, 4}] or [ '111' IN ('1xx') ] *)
   | SAMPLE (** [SAMPLE (bits(8))] *)
   | ESlice of expr * expr list (** [MEM[address+:length]] *)
   | EFields of expr * string list (** [FLAGS.[Z, B]] *)
   | ETuple of expr list
   | EArray of expr list
   | EStruct of (string * expr) list
*)

and lexpr = LEVar of identifier
(* Unsupported now:
   | LEVars of lexpr
   | LEField of lexpr * identifier
   | LEFields of lexpr * identifier list
   | LETuple of lexpr list
   | LESlice of lexpr * slice list
   | LSetter of identifier * expr list (* As in FunCall *)
*)

and stmt =
  | SPass
  | SThen of stmt * stmt
  | SAssign of lexpr * expr
  | SCond of expr * stmt * stmt

(* Unsupported now:
   | SFuncall of string * expr list
   | SReturn of expr option
   | SAssert of expr
   | SThrow of expr option
   | SCase
   | SFor
   | SWhile
   | SRepeat
   | STry
   | SPragma
*)
and subpgm = string list * stmt

let stmt_from_list = function
  | [] -> SPass
  | h :: t -> List.fold_left (fun s1 s2 -> SThen (s1, s2)) h t

let is_literal = function ELiteral _ -> true | _ -> false

let rec uses_expr = function
  | ELiteral _ -> IdSet.empty
  | EVar x -> IdSet.singleton x
  | EUnop (_, e) -> uses_expr e
  | EBinop (e1, _, e2) -> IdSet.union (uses_expr e1) (uses_expr e2)

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
      IdSet.union (defs_expr e) @@ IdSet.union (defs_stmt s1) (defs_stmt s2)

open Format

let pp_print_binop f (b : binop) =
  match b with
  | BAnd -> pp_print_string f "&&"
  | BOr -> pp_print_string f "||"
  | BImpl -> pp_print_string f "-->"
  | BEq -> pp_print_string f "<->"
  | Eq -> pp_print_string f "=="
  | NEq -> pp_print_string f "!="
  | GT -> pp_print_string f ">"
  | Geq -> pp_print_string f ">="
  | LT -> pp_print_string f "<"
  | Leq -> pp_print_string f "<="
  | Plus -> pp_print_string f "+"
  | Minus -> pp_print_string f "-"
  | Or -> pp_print_string f "OR"
  | And -> pp_print_string f "AND"
  | EOR -> pp_print_string f "EOR"
  | Mult -> pp_print_string f "*"
  | RDiv -> pp_print_string f "/"
  | DIV -> pp_print_string f "DIV"
  | MOD -> pp_print_string f "MOD"
  | LSh -> pp_print_string f "<<"
  | RSh -> pp_print_string f ">>"
  | Pow -> pp_print_string f "^"
  | App -> pp_print_string f "++"

let pp_print_unop f = function
  | UMinus -> pp_print_string f "-"
  | UBNeg -> pp_print_string f "!"
  | UNot -> pp_print_string f "NOT"

let rec pp_print_expr f e =
  match e with
  | EUnop (o, e) -> fprintf f "@[%a %a@]" pp_print_unop o pp_print_expr e
  | EBinop (e1, o, e2) ->
      fprintf f "@[<2>%a@ %a %a@]" pp_print_expr e1 pp_print_binop o
        pp_print_expr e2
  | EVar x -> pp_print_string f x
  | ELiteral v -> Values.pp_print_value f v

and pp_print_lexpr f e = match e with LEVar x -> pp_print_string f x

and pp_print_stmt f s =
  match s with
  | SPass -> pp_print_string f "Pass"
  | SAssign (le, e) ->
      fprintf f "@[<3>%a =@ %a@]" pp_print_lexpr le pp_print_expr e
  | SThen (s1, s2) ->
      fprintf f "@[<v 0>%a ;@;%a@]" pp_print_stmt s1 pp_print_stmt s2
  | SCond (e, s1, s2) ->
      fprintf f "@[<3>@[<h>if@ %a@ then@]@ %a@ else@ %a@]" pp_print_expr e
        pp_print_stmt s1 pp_print_stmt s2

and pp_print_subpgm f = function
  | args, s ->
      fprintf f "@[<hov 3>@[<hv 2>fun %a@]@ => %a@]"
        (pp_print_list ~pp_sep:pp_print_space pp_print_string)
        args pp_print_stmt s
