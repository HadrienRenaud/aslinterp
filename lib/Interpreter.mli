module type S = sig
  module B : Backend.S

  type ast = B.value AST.t

  val run : ast -> B.value list -> B.value list B.m
end

module Make (B : Backend.S) : S with module B = B
