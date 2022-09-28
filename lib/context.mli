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

  val find : addr -> t -> Values.value
  (** Gives the value of a variable.

      Warning: an unbound variable will trigger a runtime error.*)

  val set : addr -> Values.value -> t -> t
  (** Binds the variable to its new value. *)

  val can_use : addr -> t -> bool
  (** Checks if the variable can be used at this time.

      This includes if the variable is defined here or not. *)

  val can_set : addr -> t -> bool
  (** Checks if the varaible can be set at this time. *)

  val pp_print : Format.formatter -> t -> unit
  (** A formatting function for this context. *)
end

(********************************************************************************************)
(** {2 Provided contexts} *)

module SequentialContext : CONTEXT
(** The normal sequential context. *)
