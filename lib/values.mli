type bitstring = bool array

(** Type of indexing values, either in ASL tuples, ASL arrays or ASL structures. *)
type indexes = IInt of Z.t | IString of string

type 'a map = (indexes * 'a) list
(** Underlying object behind a Map. It is for now an association list. *)

(** Values in CoreASL, ie values from ASL, where compound values are mapped to Map. *)
type value =
  | Bitstr of bitstring  (** Bit strings of fixed size *)
  | Int of Z.t  (** Integers, unbounded in size, signed *)
  | Real of Q.t
      (** Real number in the mathematical sense, unbounded in size or precision *)
  | Bool of bool  (** Boolean *)
  | String of string  (** Strings *)
  | Map of value map  (** Maps *)

val pp_print_value : Format.formatter -> value -> unit
val bitstring_of_z : Z.t -> int -> bitstring
val z_of_bitstring : bitstring -> Z.t
val make_int : int -> value
val make_real : float -> value
val make_bitstring : int -> int -> value
