module Structure : Map.S with type key = string

type 'a structure = 'a Structure.t
type bitstring = bool array

(** Values from the spec, section K16.3 *)
type value =
  | Bitstr of bitstring  (** Bit strings of fixed size *)
  | Int of int  (** Integers, unbounded in size, signed *)
  | Real of float
      (** Real number in the mathematical sense, unbounded in size or precision *)
  (* TODO: investigate if there is a possibility for real number with unbounded precision in caml *)
  | Bool of bool  (** Boolean *)
  | Enum of string  (** Enumeration, ie a member of a set of named values *)
    (* as a named value cannot be shared between different enumerations, we do not have to specify of which enumeration we are talking. *)
  | Struct of value structure
      (** Structures: a compoud value composed of one or more value items *)
  | Tuple of value array  (** Tuples, an ordered set of values *)
  | Array of value array

val pp_print_value : Format.formatter -> value -> unit
val bitstring_of_int : int -> int -> bitstring
val int_of_bitstring : bitstring -> int
val int_pow : int -> int -> int
