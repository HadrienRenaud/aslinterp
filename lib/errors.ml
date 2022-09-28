
type error =
  | DivisionByZero
  | UnsupportedOperation of string
  | SemanticError of string
  | UndefinedVariable of string
  | InterpretorError of string
  | BlockedInterpretor

let pp_print_error f e =
  match e with
  | DivisionByZero -> Format.pp_print_string f "Division by zero"
  | UnsupportedOperation s -> Format.fprintf f "Unsupported operation: %s" s
  | SemanticError s -> Format.fprintf f "Semantic error: %s" s
  | UndefinedVariable x -> Format.fprintf f "Variable %s is undefined." x
  | InterpretorError s -> Format.fprintf f "Internal error: %s" s
  | BlockedInterpretor -> Format.pp_print_string f "Blocked interpretor"

type 'a result = ('a, error) Result.t
