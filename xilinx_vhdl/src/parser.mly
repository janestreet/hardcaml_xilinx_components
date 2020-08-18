%{
(** Parser for Xilinx VCOMP VHDL component specifications. *)
(*  Written a long, long time ago for the HDFS project *)

%}

%token P_component
%token P_is
%token P_end
%token <string> P_ident
%token P_attribute
%token P_of
%token P_ns
%token P_ps
%token P_in
%token P_inout
%token P_out
%token P_port
%token P_equals
%token P_generic
%token P_downto
%token P_to
%token P_semi
%token P_colon
%token P_obracket
%token P_cbracket
%token P_dot
%token P_others
%token P_goto
%token P_x01

%token <string> P_int
%token <string> P_char
%token <string> P_string
%token <string> P_hex_string
%token <string> P_real

%token P_plus P_minus P_cat
%token P_multiply P_divide

%left P_minus P_plus P_cat
%left P_multiply P_divide
%nonassoc NEG

%token P_std_logic_type
%token P_std_logic_vector_type
%token P_bit_type
%token P_bit_vector_type
%token P_integer_type
%token P_real_type
%token P_bool_type
%token P_string_type
%token P_time_type

%token P_library
%token P_use
%token P_signal
%token P_package

%token P_endfile

%type <Vhdl.Component.t list> main
%start main

%%

main:
    header component_list P_end P_ident P_semi
                                            { $2 }

use_clause:
    P_use P_ident P_dot P_ident P_dot P_ident P_semi
                                            { }

use_clauses:
      use_clause                            { }
    | use_clause use_clauses                { }

header:
    P_library P_ident P_semi
    use_clauses
    P_package P_ident P_is
                                            { }

signal:
    P_signal P_ident P_colon vhdtype P_semi { }
  | P_signal P_ident P_colon vhdtype P_colon P_equals expr_or_others P_semi
                                            { }

component_list:
                                            { [] }
  | component_list component                { $1 @ [$2] }
  | component_list attribute                { $1 }
  | component_list signal                   { $1 }

component:
    P_component P_ident generics ports P_end P_component P_semi
                                            {
                                              { name = $2
                                              ; generics = $3
                                              ; ports = $4
                                              }
                                            }

attribute:
    P_attribute P_ident P_colon P_string_type P_semi
                                            { }
  | P_attribute P_ident P_of P_ident P_colon P_component P_is P_string P_semi
                                            { }

generics:
                                            { [] }
  | P_generic P_obracket ports_list P_cbracket P_semi
                                            { $3 }

ports:
                                            { [] }
  | P_port P_obracket ports_list P_cbracket P_semi
                                            { $3 }

ports_list:
    port_decl                               { [$1] }
  | ports_list P_semi port_decl             { $1 @ [$3] }

port_decl:
    P_ident P_colon dirn vhdtype            {
                                              { name = $1
                                              ; dir = $3
                                              ; type_ = $4
                                              ; default = None
                                              }
                                            }
  | P_ident P_colon dirn vhdtype P_colon P_equals expr_or_others timespec
                                            {
                                              { name = $1
                                              ; dir = $3
                                              ; type_ = $4
                                              ; default = Some($7) (* timespec *)
                                              }
                                            }

vhdtype:
    P_std_logic_type                        { Std_logic }
  | P_std_logic_vector_type range           { Std_logic_vector($2) }
  | P_bit_type                              { Bit }
  | P_bit_vector_type range                 { Bit_vector($2) }
  | P_integer_type                          { Integer }
  | P_real_type                             { Real }
  | P_bool_type                             { Boolean }
  | P_string_type                           { String }
  | P_time_type                             { Time }

timespec:
                                            { "" }
  | P_ps                                    { " ps" }
  | P_ns                                    { " ns" }

range:
                                            { None }
  | P_obracket expr P_downto expr P_cbracket
                                            { Some($2, Down, $4) }
  | P_obracket expr P_to expr P_cbracket
                                            { Some($2, Up, $4) }

expr:
    P_char                                  { Char $1 }
  | P_int                                   { Int (int_of_string $1) }
  | P_real                                  { Float (float_of_string $1) }
  | P_ident                                 { Id $1 }
  | P_string                                { String $1 }
  | P_hex_string                            { Hex $1 }
  | P_obracket expr P_cbracket              { $2 }
  | expr P_plus expr                        { Op("+", $1, $3) }
  | expr P_minus expr                       { Op("-", $1, $3) }
  | expr P_cat expr                         { Op("&", $1, $3) }
  | expr P_multiply expr                    { Op("*", $1, $3) }
  | expr P_divide expr                      { Op("/", $1, $3) }
  | P_minus expr %prec NEG                  { Neg($2) }

expr_or_others:
    expr                                    { $1 }
  | P_obracket P_others P_goto P_char P_cbracket
                                            { Others $4 }
  | P_x01 P_obracket P_ident P_cbracket     { X01 $3 }

dirn:
                                            { In }
  | P_in                                    { In }
  | P_inout                                 { Inout }
  | P_out                                   { Out }
