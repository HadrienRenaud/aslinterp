(** The semantics of ASL *)

(** {2 Prolog} *)

type error =
  | DivisionByZero
  | UnsupportedOperation of string
  | SemanticError of string
  | UndefinedVariable of string
  | InterpretorError of string

val pp_print_error : Format.formatter -> error -> unit

type 'a result = ('a, error) Result.t

(** {2 Semantics of operations } *)

val eval_binop :
  Values.value -> Syntax.binop -> Values.value -> Values.value result
(** Evaluate an binary operation. *)

val eval_unop : Syntax.unop -> Values.value -> Values.value result
(** Evaluate an unary operation. *)

(** {2 Reduction contexts} *)

type context = {
  v : Values.value Syntax.IdMap.t;
  s : Syntax.subpgm Syntax.IdMap.t;
  d : Syntax.IdSet.t;
  u : Syntax.IdSet.t;
}
(** context is the reduction environment. *)

val ctx_find_opt_var : Syntax.identifier -> context -> Values.value option
(** Maps a variable name to its value in this context.
    Returns None if the variable is not defined. *)

val ctx_empty : context
(** The empty context. *)

val ctx_update_var : Syntax.identifier -> Values.value -> context -> context
val pp_print_context : Format.formatter -> context -> unit

(** {2 Semantics of expressions } *)

val do_one_step_expr : context -> Syntax.expr -> (context * Syntax.expr) result
val eval_expr : context -> Syntax.expr -> (context * Values.value) result

(** {2 Semantics of statements } *)

val do_one_step_stmt : context -> Syntax.stmt -> (context * Syntax.stmt) result
val eval_stmt : context -> Syntax.stmt -> context result
