(** The semantics of ASL *)

(** {2 Semantics of operations } *)

val eval_binop :
  Values.value -> Syntax.binop -> Values.value -> Values.value Errors.result
(** Evaluate an binary operation. *)

val eval_unop : Syntax.unop -> Values.value -> Values.value Errors.result
(** Evaluate an unary operation. *)

(** {2 Interpreter functor} *)
module type INTERPRETER = sig
  type context

  (** {3 Semantics of expressions } *)
  val eval_expr :
    context -> Syntax.expr -> (context * Values.value) Errors.result
  (** Evaluates an expression inside a context. *)

  (** {3 Semantics of statements }

    Here come the real interpreter! *)

  val do_one_step_stmt :
    context -> Syntax.stmt -> (context * Syntax.stmt) Errors.result
  (** A small-step transition function which follows the semantic rules on the reduction of
    statements.

    We try to keep as close as possible to the
    semantic rules. It uses [eval_expr] for the reduction of expressions, as
    specified by the semantic rules. *)

  val eval_stmt : context -> Syntax.stmt -> context Errors.result
  (** The interpreter in itself. It works by taking the transitive closure of the
    [do_one_step_stmt] transition function. *)
end

module MakeInterpreter (Ctx : Context.CONTEXT) :
  INTERPRETER with type context = Ctx.t

(** {2 Interpreters} *)

module SequentialInterpreter :
  INTERPRETER with type context = Context.SequentialContext.t
