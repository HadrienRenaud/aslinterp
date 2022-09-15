open Aslinterp.Values

let () =
  pp_print_value Format.std_formatter
    (Struct
       (Structure.of_seq
       @@ List.to_seq
            [
              ("tuple", Tuple (Array.of_list [ Int 3; Bool true ]));
              ("enum", Enum "enum.possibility");
              ("array", Array (Array.of_list [ Int 5; Real 3.4 ]));
              ("bitstring", Bitstr (Array.of_list [ true; true; false ]));
            ]))

let () = Format.print_newline ()
let () = print_endline "Hello, World!"
