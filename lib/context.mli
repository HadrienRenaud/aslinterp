(********************************************************************************************)
(** {2 Context modules} *)

(** The module signature [CONTEXT] is the abstraction over the different contexts possible. *)
module type CONTEXT = sig
  type t
  (** The type of the context *)

  val empty : t
  (** The empty context *)

  val find :
    Syntax.identifier -> Values.address -> t -> Values.value Errors.result
  (** Gives the value of a variable. *)

  val set :
    Syntax.identifier -> Values.address -> Values.value -> t -> t Errors.result
  (** Binds the variable to its new value. *)

  val find_subpgm : Syntax.identifier -> t -> Syntax.subpgm Errors.result
  (** Gives the value of a stored subprogram. *)

  val pop_locals : t -> t
  (** Gives a fresh version of the context where all locals have been removed. *)

  val with_globals_from : t -> t -> t
  (** [with_globals_from a b] returns a context with the globals of a and the locals of b *)

  val pp_print : Format.formatter -> t -> unit
  (** A formatting function for this context. *)

  val of_globals :
    (Syntax.identifier * Values.value) Seq.t -> Syntax.subpgm Seq.t -> t
  (** A constructor that takes a serie of global values and a serie of functions to populate into the context. *)
end

(********************************************************************************************)
(** {2 Provided contexts} *)

module SequentialContext : CONTEXT
(** The normal sequential context. *)

module Logger (Ctx : CONTEXT) : CONTEXT with type t = Ctx.t
