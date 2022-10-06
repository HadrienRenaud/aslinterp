type error =
  | DivisionByZero
  | TypeError of string
  | UnsupportedOperation of string
  | SemanticError of string
  | UndefinedVariable of string
  | IndexOutOfBounds of string
  | InterpreterError of string
  | BlockedInterpreter

let pp_print_error f e =
  match e with
  | DivisionByZero -> Format.pp_print_string f "Division by zero"
  | TypeError s -> Format.fprintf f "Type error: %s" s
  | UnsupportedOperation s -> Format.fprintf f "Unsupported operation: %s" s
  | SemanticError s -> Format.fprintf f "Semantic error: %s" s
  | UndefinedVariable x -> Format.fprintf f "Variable %s is undefined." x
  | IndexOutOfBounds s -> Format.fprintf f "IndexOutOfBounds: %s" s
  | InterpreterError s -> Format.fprintf f "Internal error: %s" s
  | BlockedInterpreter -> Format.pp_print_string f "Blocked interpreter"

type 'a result = ('a, error) Result.t
