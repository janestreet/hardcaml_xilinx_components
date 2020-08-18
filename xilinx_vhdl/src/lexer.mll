(** Lexer for Xilinx VCOMP VHDL component specifications. *)
(*  Written a long, long time ago for the HDFS project *)
{

open! Import
open Parser
open Lexing

(* VHDL tokens *)
let keyword_table = Hashtbl.create (module String)
let () =
  List.iter
    ~f:(fun (kwd, tok) -> Hashtbl.add_exn keyword_table ~key:kwd ~data:tok)
    [
      "component",            P_component;
      "is",                   P_is;
      "end",                  P_end;
      "attribute",            P_attribute;
      "of",                   P_of;
      "port",                 P_port;
      "generic",              P_generic;
      "downto",               P_downto;
      "to",                   P_to;
      "others",               P_others;
      "to_x01",               P_x01;

      "ps",                   P_ps;
      "ns",                   P_ns;

      "in",                   P_in;
      "inout",                P_inout;
      "out",                  P_out;

      "std_logic",            P_std_logic_type;
      "std_ulogic",           P_std_logic_type;
      "std_logic_vector",     P_std_logic_vector_type;
      "integer",              P_integer_type;
      "real",                 P_real_type;
      "boolean",              P_bool_type;
      "string",               P_string_type;
      "bit",                  P_bit_type;
      "bit_vector",           P_bit_vector_type;
      "time",                 P_time_type;

      "library",              P_library;
      "use",                  P_use;
      "signal",               P_signal;
      "package",              P_package;
    ]

let lineNum = ref 1
let init_linenum () = lineNum := 1
let incr_linenum () = lineNum := !lineNum + 1

}

let newline = ('\n' | '\r' '\n')
let not_newline = [^ '\r' '\n' ]
let digit = ['0'-'9']

rule token = parse

  | [' ' '\t']              { token lexbuf }                      (* skip blanks *)
  | '-''-'not_newline*newline
                            { incr_linenum (); token lexbuf }     (* skip single line comments *)
  | newline                 { incr_linenum (); token lexbuf }
  | '=''>'                  { P_goto }
  | ';'                     { P_semi }
  | ':'                     { P_colon }
  | '('                     { P_obracket }
  | ')'                     { P_cbracket }
  | '='                     { P_equals }
  | '.'                     { P_dot }

  | '+'                     { P_plus }
  | '-'                     { P_minus }
  | '*'                     { P_multiply }
  | '/'                     { P_divide }

  | '&'                     { P_cat }

  (* integer *)
  | ['0'-'9']+              { let lxm = lexeme lexbuf in P_int lxm }

  (* char constant *)
  | '\'' ['A'-'Z' 'a'-'z' '0'-'9' ] * '\''
                            { let lxm = lexeme lexbuf in
                              P_char (String.sub lxm ~pos:1 ~len:(String.length lxm - 2)) }

  (* hex string constant *)
  | 'X''"' ['A'-'F' 'a'-'f' '0'-'9' '_' ] * '"'
                            { let lxm = lexeme lexbuf in
                              P_hex_string (String.sub lxm ~pos:2 ~len:(String.length lxm - 3)) }

  (* string constant *)
  | '"' ['A'-'Z' 'a'-'z' '0'-'9' '_' '/' '\\' '.'] * '"'
                            { let lxm = lexeme lexbuf in
                              P_string (String.sub lxm ~pos:1 ~len:(String.length lxm - 2)) }

  (* floating point *)
  | digit+('.'digit+)?(['e''E']digit+)?
                            { let lxm = lexeme lexbuf in P_real lxm }

  (* token or identifier *)
  | ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_'] *
                            { let id = lexeme lexbuf in
                              let id_lwr = String.lowercase id in
                              try let tok = Hashtbl.find_exn keyword_table id_lwr in tok
                              with Not_found_s _ | Caml.Not_found -> P_ident id }

  | eof                     { P_endfile }
