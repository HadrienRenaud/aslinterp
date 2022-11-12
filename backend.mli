module type S = sig
  type vint
  type vbool
  type vreal
  type vbitvector
  type 'a m
  type loc
  type value = (vint, vbool, vreal, vbitvector) AST.value
  type scope = AST.identifier * int

  val vint_of_int : int -> vint
  val bind_data : 'a m -> ('a -> 'b m) -> 'b m
  val bind_seq : 'a m -> ('a -> 'b m) -> 'b m
  val prod : 'a m -> 'b m -> ('a * 'b) m

  val choice : value m -> 'b m -> 'b m -> 'b m
  (** choice is a boolean if operator *)

  val return : 'a -> 'a m
  val failwith : string -> 'a m
  val binop : AST.binop -> value -> value -> value m
  val unop : AST.unop -> value -> value m
  val on_write_identifier : AST.identifier -> scope -> value -> unit m
  val on_read_identifier : AST.identifier -> scope -> value -> unit m
end
