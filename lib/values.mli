type bitstring = bool array

(** Type of indexing values, either in ASL tuples, ASL arrays or ASL structures. *)
type index = IInt of Z.t | IString of string

type address = index list
(** Type of nested indexing values for Map cases. *)

type 'a map = (index * 'a) list
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

val find_address_in_value : value -> address -> value Errors.result
(** Find the value referenced by the address into the value tree. *)

val set_address_in_value : value -> address -> value -> value Errors.result
(** [set_address_in_value obj addr new_value] returns [obj] with [new_value] referenced by [addr]. *)

val pp_print_index : Format.formatter -> index -> unit
val pp_print_value : Format.formatter -> value -> unit
val pp_print_address : Format.formatter -> address -> unit
val bitstring_of_z : Z.t -> int -> bitstring
val z_of_bitstring : bitstring -> Z.t
val make_int : int -> value
val make_real : float -> value
val make_bitstring : int -> int -> value
val make_array : value list -> value
