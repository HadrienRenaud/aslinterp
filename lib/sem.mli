(** The semantics of ASL *)

(** {2 Prolog} *)

type error =
  | DivisionByZero
  | UnsupportedOperation of string
  | SemanticError of string
  | UndefinedVariable of string
  | InterpretorError of string
  | BlockedInterpretor

val pp_print_error : Format.formatter -> error -> unit

type 'a result = ('a, error) Result.t
type context = Context.SequentialContext.t

(** {2 Semantics of operations } *)

val eval_binop :
  Values.value -> Syntax.binop -> Values.value -> Values.value result
(** Evaluate an binary operation. *)

val eval_unop : Syntax.unop -> Values.value -> Values.value result
(** Evaluate an unary operation. *)

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
