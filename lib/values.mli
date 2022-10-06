(**
{2 Types}

{3 Singular types}

These types are supported by native ocaml objects, or [ZArith] integers or rationnals, except for the bitvector type:
*)

type bitvector = bool list
(** The bitvector type *)

(** {3 Compound types} *)

type record = (string * value) list
(** A record is a type with fixed fields *)

and varray = (Z.t * value) list
(** An array that stores values.

It is a sparse array as we might have to represent entire memory system with it. *)

and tuple = value list
(** A tuple, mainly used for returning multiple values from a function. *)

(** Values in CoreASL, ie values from ASL, where compound values are mapped to Map. *)
and value =
  | VBitVec of bitvector  (** Bit strings of fixed size *)
  | VInt of Z.t  (** Integers, unbounded in size, signed *)
  | VReal of Q.t
      (** Real number in the mathematical sense, unbounded in size or precision *)
  | VBool of bool  (** Boolean *)
  | VString of string  (** Strings *)
  | VEnum of string  (** Enums *)
  | VTuple of tuple  (** Tuples **)
  | VRecord of record  (** Record types *)
  | VArray of varray  (** Array *)

(** {2 Nested compound types manipulation and adresses} *)

type address = value list
(** Type of indexing values for nested composed ASL types. *)

val find_address_in_value : value -> address -> value Errors.result
(** Find the value referenced by the address into the value tree. *)

val set_address_in_value : value -> address -> value -> value Errors.result
(** [set_address_in_value obj addr new_value] returns [obj] with [new_value] referenced by [addr]. *)

(** {2 Utils}

    {3 Printer} *)

val pp_print_value : Format.formatter -> value -> unit
(** A [Format]-style pretty printer for values. *)

(** {3 Conversion utilities} *)

val bitvector_of_z : Z.t -> int -> bitvector
(** Create a bitvector of specified length from a [ZArith.Z.t] *)

val z_of_bitvector : bitvector -> Z.t
(** Create a Z.t from a bitvector, inverse of [bitvector_of_z]. *)

(** {3 Values creators} *)

val make_int : int -> value
val make_real : float -> value
val make_bitvector : int -> int -> value
