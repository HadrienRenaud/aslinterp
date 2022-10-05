(** Syntax of ASL inside the interpretor.

    Defined by ARM asl-spec.
*)

(** {2 Main types}

    {3 Operations}*)

(** The different operations on two expressions.

    Ref: Section 5.2 *)
type binop =
  (* Boolean operations *)
  | BAnd  (** [&&] *)
  | BOr  (** [||] *)
  | BImpl  (** [-->] *)
  | BEq  (** [<->] *)
  (* Comparisons *)
  | Eq  (** [==] *)
  | NEq  (** [!=] *)
  | GT  (** [>] *)
  | Geq  (** [>=] *)
  | LT  (** [<] *)
  | Leq  (** [<=] *)
  (* Arithmetic and logic *)
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

(** The different operations on one expression. *)
type unop = UMinus  (** [-] *) | UBNeg  (** [!] *) | UNot  (** [NOT] *)

(** {3 Expressions} *)

type identifier = string
(** The type of named element in the program, such as variables, functions, fields .. *)

module IdMap : Map.S with type key = identifier
module IdSet : Set.S with type elt = identifier

(** Expressions as defined by chapter 5. *)
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

(** {3 Statements} *)

and lexpr = LEVar of identifier
(* Unsupported now:
   | LEVars of lexpr
   | LEField of lexpr * identifier
   | LEFields of lexpr * identifier list
   | LETuple of lexpr list
   | LESlice of lexpr * slice list
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

(** {2 Utilities} *)

val stmt_from_list : stmt list -> stmt
(** Constructs a statement from a list of statements by putting them in a tree of [SThen] *)

val is_literal : expr -> bool
(** Returns true if the argument is a literal, false otherwise *)

(** {3 Formatters} *)

val pp_print_unop : Format.formatter -> unop -> unit
val pp_print_binop : Format.formatter -> binop -> unit
val pp_print_expr : Format.formatter -> expr -> unit
val pp_print_lexpr : Format.formatter -> lexpr -> unit
val pp_print_stmt : Format.formatter -> stmt -> unit
