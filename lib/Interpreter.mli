module type S = sig
  module B : Backend.S

  type ast = B.value AST.t
  type sfunc = B.value list -> B.value list B.m

  val run : ast -> (string * sfunc) list -> B.value list -> B.value list B.m
  (** [run spec_lib ast args] runs the function main of the ast, in an environment build from the ast and spec_lib. *)
end

module Make (B : Backend.S) : S with module B = B
