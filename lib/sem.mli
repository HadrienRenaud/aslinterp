(** The semantics of ASL *)

(** {2 Semantics of operations } *)

val eval_binop :
  Values.value -> Syntax.binop -> Values.value -> Values.value Errors.result
(** Evaluate an binary operation. *)

val eval_unop : Syntax.unop -> Values.value -> Values.value Errors.result
(** Evaluate an unary operation. *)

(** {2 Interpretor functor} *)
module type INTERPRETOR = sig
  type context

  (** {3 Semantics of expressions } *)

  val do_one_step_expr :
    context -> Syntax.expr -> (context * Syntax.expr) Errors.result
  (** A small-step transition function, that we try to keep as close as possible to the
    semantic rules. *)

  val eval_expr :
    context -> Syntax.expr -> (context * Values.value) Errors.result
  (** A evaluation function for expressions, that uses the function [do_one_step_expr] and take
    its transitive closure. *)

  (** {3 Semantics of statements }

    Here come the real interpretor! *)

  val do_one_step_stmt :
    context -> Syntax.stmt -> (context * Syntax.stmt) Errors.result
  (** A small-step transition function which follows the semantic rules on the reduction of
    statements.

    We try to keep as close as possible to the
    semantic rules. It uses [do_one_step_expr] for the reduction of expressions, as
    specified by the semantic rules. *)

  val eval_stmt : context -> Syntax.stmt -> context Errors.result
  (** The interpretor in itself. It works by taking the transitive closure of the
    [do_one_step_stmt] transition function. *)
end

module type SEMANTIC_DESCRIPTOR = sig
  val semi_column_concurrent : bool
end

module SequentialSemantics : SEMANTIC_DESCRIPTOR

module MakeInterpretor (Ctx : Context.CONTEXT) (_ : SEMANTIC_DESCRIPTOR) :
  INTERPRETOR with type context = Ctx.t

(** {Interpretors} *)

module SequentialInterpretor :
  INTERPRETOR with type context = Context.SequentialContext.t
