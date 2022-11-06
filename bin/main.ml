open Aslinterp

(* Taken from herdtools7/lib/Pos.ml *)
let pp_pos chan pos =
  let open Lexing in
  Printf.fprintf chan "File \"%s\", line %i, character %i" pos.pos_fname
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol)

let build_ast_from_file f =
  let lexbuf = Lexing.from_channel (open_in f) in
  let () = Lexing.set_filename lexbuf f in
  try Parser.ast Lexer.token lexbuf with
  | Parser.Error ->
      Printf.eprintf "%a: Cannot parse." pp_pos lexbuf.lex_curr_p;
      exit 1
  | LexMisc.Error (_msg, pos) ->
      Printf.eprintf "%a: unknown token." pp_pos pos;
      exit 1

let exec ast =
  let open Native in
  match NativeInterpreter.run ast [] [] () with
  | Ok _li -> Printf.printf "Ran ok.\n"
  | Error err -> Printf.printf "%a\n" pp_err err

let () =
  let f = Sys.argv.(1) in
  let () = Printf.printf "\rParsing and running %s...            \n" f in
  let parsed_ast = build_ast_from_file f in
  let native_ast = Native.of_parsed_ast parsed_ast in
  exec native_ast
