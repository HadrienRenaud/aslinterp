type bitvector = bool list

(** Values in CoreASL, ie values from ASL, where compound values are mapped to Map. *)
type value =
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

and record = (string * value) list
and varray = (Z.t * value) list
and tuple = value list

val pp_print_value : Format.formatter -> value -> unit
val bitvector_of_z : Z.t -> int -> bitvector
val z_of_bitvector : bitvector -> Z.t
val make_int : int -> value
val make_real : float -> value
val make_bitvector : int -> int -> value
val find_in_value : value -> value list -> value Errors.result
val set_in_value : value -> value list -> value -> value Errors.result
