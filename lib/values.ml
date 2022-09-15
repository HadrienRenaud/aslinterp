type value = Int of int

let print_value v = match v with
 | Int x -> print_int x
