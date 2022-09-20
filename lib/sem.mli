(** The semantics of ASL *)

module SMap : Map.S with type key = string
(** {2 Prolog} *)

type error =
  | DivisionByZero
  | UnsupportedOperation of string
  | SemanticError of string
  | UndefinedVariable of string

val pp_print_error : Format.formatter -> error -> unit

type 'a result = ('a, error) Result.t

(** {2 Semantics of operations } *)

val eval_binop :
  Values.value -> Syntax.binop -> Values.value -> Values.value result
(** Evaluate an binary operation. *)

val eval_unop : Syntax.unop -> Values.value -> Values.value result
(** Evaluate an unary operation. *)

(** {2 Reduction contexts} *)

type context = { v : Values.value SMap.t; s : Syntax.subpgm SMap.t }
(** context is the reduction environment. *)

val ctx_find_opt_var : Syntax.identifier -> context -> Values.value option
(** Maps a variable name to its value in this context.
    Returns None if the variable is not defined. *)

val ctx_empty : context

(** {2 Semantics of expressions } *)

type expr_reduction_status =
  | Finished of context * Values.value
  | OneStepDone of context * Syntax.expr
  | Err of error

val do_one_step_expr : context -> Syntax.expr -> expr_reduction_status

val eval_expr :
  context -> Syntax.expr -> (context * Values.value, error) Result.t
