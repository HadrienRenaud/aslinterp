(********************************************************************************************)
(** {2 Context modules} *)

(** The module signature [CONTEXT] is the abstraction over the different contexts possible. *)
module type CONTEXT = sig
  type t
  (** The type of the context *)

  type addr = Syntax.identifier
  (** Variables *)

  val empty : t
  (** The empty context *)

  val find : addr -> t -> Values.value Errors.result
  (** Gives the value of a variable.

      Warning: an unbound variable will trigger a runtime error.*)

  val set : addr -> Values.value -> t -> t Errors.result
  (** Binds the variable to its new value. *)

  val pp_print : Format.formatter -> t -> unit
  (** A formatting function for this context. *)
end

(********************************************************************************************)
(** {2 Provided contexts} *)

module SequentialContext : CONTEXT
(** The normal sequential context. *)

module Logger (Ctx : CONTEXT) : CONTEXT with type t = Ctx.t
