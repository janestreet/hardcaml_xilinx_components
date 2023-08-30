open! Import

let parse lexbuf =
  Lexer.init_linenum ();
  match Parser.main Lexer.token lexbuf with
  | x -> Ok x
  | exception _ -> error_s [%message "parse error" ~at_line:(!Lexer.lineNum : int)]
;;

let parse_string string = parse (Lexing.from_string string)
let parse_in_channel in_channel = parse (Lexing.from_channel in_channel)
