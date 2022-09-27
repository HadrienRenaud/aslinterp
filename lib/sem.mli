(** The semantics of ASL *)

(** {2 Prolog} *)

type error =
  | DivisionByZero
  | UnsupportedOperation of string
  | SemanticError of string
  | UndefinedVariable of string
  | InterpretorError of string
  | BlockedInterpretor
  | OutOfBoundError of int * int

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
      (** Maps the variable names to their values *)
  s : Syntax.subpgm Syntax.IdMap.t;
      (** Maps the function names to their definition *)
  d : Syntax.IdSet.t;
      (** For concurrent programs, [d] represents the variables that should be defined at this
      point, but are not yet defined. You cannot either define or use variables that are in
      [d]. *)
  u : Syntax.IdSet.t;
      (** For concurrent programs, [u] represents the variables that are yet to be used by the
      program. YOu cannot redefine them until they are used. *)
}
(** context is the reduction environment. *)

val ctx_find_opt_var : Syntax.identifier -> context -> Values.value option
(** Maps a variable name to its value in this context.
    Returns None if the variable is not defined. *)

val ctx_empty : context
(** The empty context. *)

val ctx_update_var : Syntax.identifier -> Values.value -> context -> context
(** Add new value bind to var in a context *)

val ctx_update_array :
  Syntax.identifier -> int -> Values.value -> context -> context result
(** Bind index i of array to a new value in context. *)

(** {4 Concurrent helpers}

    Those do not interfere with execution in the sequential case.

    In the concurrent case, "eagerly modify" means modify in the following (in the sense of
    [;]) of the program.
 *)

val ctx_can_use_var : Syntax.identifier -> context -> bool
(** Check if the variable can be used in the context.

    Does not check if the variable is defined, only if it is not restricted in a
    concurrent way. *)

val ctx_can_set_var : Syntax.identifier -> context -> bool
(** Check if the variable can be set in the context.

    Does not check if the name is not already taken by a function nor a unsettable variable.
*)

val ctx_can_set_array : Syntax.identifier -> int -> context -> bool
(** Check if the ith case of the variable can be set in context. *)

val uses_expr : Syntax.expr -> Syntax.IdSet.t
(** Determines what cannot be eagerly modified based on what variables this expression uses.
*)

val uses_stmt : Syntax.stmt -> Syntax.IdSet.t
(** Determines what cannot be eagerly modified based on what variables this statement uses. *)

val uses_lexpr : Syntax.lexpr -> Syntax.IdSet.t
(** Determines what cannot be eagerly modified based on what variables this left-expr uses. *)

val defs_expr : Syntax.expr -> Syntax.IdSet.t
(** Determines what cannot be eagerly fetched from context based on what variables this
    expression defines. *)

val defs_lexpr : Syntax.lexpr -> Syntax.IdSet.t
(** Determines what cannot be eagerly fetched from context based on what variables this
    left-expr defines. *)

val defs_stmt : Syntax.stmt -> Syntax.IdSet.t
(** Determines what cannot be eagerly fetched from context based on what variables this
    statement defines. *)

val ctx_expand_uses_defs : Syntax.stmt -> context -> context
(** Expand the context guards to take into account [s].
    It uses [uses_stmt] and [defs_stmt]. *)

val ctx_discard_uses_defs : context -> context -> context
(** [ctx_discard_uses_defs previous modified] returns a context formed by discarding the
    guards expanded by [ctx_expand_uses_defs], which are taken from [previous] in the result,
    and keeping the variable parts. *)

(** {4 Utilities} *)

val pp_print_context : Format.formatter -> context -> unit
(** A pretty printer *)

(** {2 Semantics of expressions } *)

val do_one_step_expr : context -> Syntax.expr -> (context * Syntax.expr) result
(** A small-step transition function, that we try to keep as close as possible to the
    semantic rules. *)

val eval_expr : context -> Syntax.expr -> (context * Values.value) result
(** A evaluation function for expressions, that uses the function [do_one_step_expr] and take
    its transitive closure. *)

(** {2 Semantics of statements }

    Here come the real interpretor! *)

val do_one_step_stmt : context -> Syntax.stmt -> (context * Syntax.stmt) result
(** A small-step transition function which follows the semantic rules on the reduction of
    statements.

    We try to keep as close as possible to the
    semantic rules. It uses [do_one_step_expr] for the reduction of expressions, as
    specified by the semantic rules. *)

val eval_stmt : context -> Syntax.stmt -> context result
(** The interpretor in itself. It works by taking the transitive closure of the
    [do_one_step_stmt] transition function. *)
