open! Import

(* OCaml keywords that should be avoided *)
let keywords = [ "do"; "done" ]

(* Construct a legal OCaml name from the VHDL name *)
let ocaml_name name =
  let name = String.lowercase name in
  if List.mem keywords name ~equal:String.equal then name ^ "_" else name
;;

let ocaml_name_ref name = "P." ^ ocaml_name name

let rec expr ?(id = Fn.id) : Vhdl.Expression.t -> string = function
  | Int i -> Int.to_string i
  | Float i -> Float.to_string i
  | String s -> "\"" ^ s ^ "\""
  | Hex s -> "X\"" ^ s ^ "\""
  | Char s -> "'" ^ s ^ "'"
  | Id i -> id i
  | Op (op, a, b) ->
    String.concat [ "("; String.concat ~sep:" " [ expr ~id a; op; expr ~id b ]; ")" ]
  | Neg a -> "-" ^ expr a
  | Others c -> "(others => '" ^ c ^ "')"
  | X01 s -> "TO_X01(" ^ s ^ ")"
;;

(* signal, port or generic value with relevant OCaml types inferred from
   the VHDL type and/or default value *)
module Inferred_port = struct
  type t =
    { port : Vhdl.Port.t
    ; ocaml_name : string
    ; default_type : string
    ; default_value : string
    ; bits : string
    ; constr : string
    }
  [@@deriving sexp_of]

  (* A generated declaration which will cause a build failure *)
  let not_implemented port =
    { port
    ; ocaml_name = ocaml_name port.name
    ; default_type = "not_implemented"
    ; default_value = "not_implemented"
    ; bits = "0"
    ; constr = "not_implemented"
    }
  ;;

  (* infer ocaml type for std_logic and std_ulogic *)
  let std_logic port =
    let create b =
      { port
      ; ocaml_name = ocaml_name port.name
      ; default_type = "Hardcaml.Logic.Std_logic.t"
      ; default_value =
          String.concat
            [ "Hardcaml.Logic.Std_logic."; b |> Logic.Std_logic.Variants.to_name ]
      ; bits = "1"
      ; constr = Hardcaml.Parameter.Value.Variants.std_logic.name
      }
    in
    let fail () =
      raise_s [%message "invalid port_std_logic value" (port : Vhdl.Port.t)]
    in
    match port.default with
    | None -> create U
    | Some e ->
      (match e with
       | X01 _ -> create L0
       | Char c when String.length c = 1 ->
         (match Logic.Std_logic.of_char_exn c.[0] with
          | c -> create c
          | exception _ -> fail ())
       | Char _ | Float _ | Hex _ | Id _ | Int _ | Neg _ | Op _ | Others _ | String _ ->
         fail ())
  ;;

  (* infer ocaml type for boolean *)
  let boolean port ~for_vhdl =
    if for_vhdl
    then (
      let create b =
        { port
        ; ocaml_name = ocaml_name port.name
        ; default_type = "bool"
        ; default_value = b
        ; bits = "1"
        ; constr = Hardcaml.Parameter.Value.Variants.bool.name
        }
      in
      match port.default with
      | Some (Id s) ->
        (match String.lowercase s with
         | "true" -> create "true"
         | "false" -> create "false"
         | _ -> raise_s [%message "port_boolean: invalid default value" (s : string)])
      | None -> create "false"
      | Some x ->
        raise_s [%message "port_boolean: invalid default type " (x : Vhdl.Expression.t)])
    else (
      let create b =
        { port
        ; ocaml_name = ocaml_name port.name
        ; default_type = "string"
        ; default_value = b
        ; bits = "1"
        ; constr = Hardcaml.Parameter.Value.Variants.string.name
        }
      in
      match port.default with
      | Some (Id s) when String.(lowercase s = "true") -> create "\"TRUE\""
      | Some (Id s) when String.(lowercase s = "false") -> create "\"FALSE\""
      | None -> create "FALSE"
      | Some x ->
        raise_s [%message "port_boolean: invalid default type " (x : Vhdl.Expression.t)])
  ;;

  (* infer ocaml type for real *)
  let real port =
    let create default_value =
      { port
      ; ocaml_name = ocaml_name port.name
      ; default_type = "float"
      ; default_value
      ; bits = "0"
      ; constr = Hardcaml.Parameter.Value.Variants.real.name
      }
    in
    match port.default with
    | Some (Float f) -> create (Float.to_string f)
    | None -> create "0."
    | _ -> failwith "port_real"
  ;;

  (* infer ocaml type for integer *)
  let integer port =
    let create default_value =
      { port
      ; ocaml_name = ocaml_name port.name
      ; default_type = "int"
      ; default_value
      ; bits = "0"
      ; constr = Hardcaml.Parameter.Value.Variants.int.name
      }
    in
    match port.default with
    | Some (Int i) -> create (Int.to_string i)
    | None -> create "0"
    | _ -> failwith "port_integer"
  ;;

  (* infer ocaml type for string *)
  let string port =
    let create default_value =
      { port
      ; ocaml_name = ocaml_name port.name
      ; default_type = "string"
      ; default_value
      ; bits = "0"
      ; constr = Hardcaml.Parameter.Value.Variants.string.name
      }
    in
    match port.default with
    | Some (String s) -> create ("\"" ^ s ^ "\"")
    | _ -> failwith "port_string"
  ;;

  (* infer ocaml type for std_logic_vector, bit_vector port *)
  let vector port (range : Vhdl.Range.t option) =
    let create bits = { (not_implemented port) with bits } in
    match range with
    | None -> failwith "port_vector: width cant be found"
    | Some (a, d, b) ->
      let hi, lo =
        match d with
        | Down -> a, b
        | Up -> b, a
      in
      create
        (sprintf
           "((%s) - (%s) + 1)"
           (expr hi ~id:ocaml_name_ref)
           (expr lo ~id:ocaml_name_ref))
  ;;

  (* infer ocaml type for std_logic_vector, bit_vector generic.  This is a bit more
     complex as the width may need to be inferred from the default value. *)
  let generic_vector port (range : Vhdl.Range.t option) ~is_bitvector =
    let create_std_logic_vector bits value =
      { port
      ; ocaml_name = ocaml_name port.name
      ; default_type = "Hardcaml.Logic.Std_logic_vector.t"
      ; default_value = "Hardcaml.Logic.Std_logic_vector.of_string" ^ value
      ; bits
      ; constr = Hardcaml.Parameter.Value.Variants.std_logic_vector.name
      }
    in
    let create_bit_vector bits value =
      { port
      ; ocaml_name = ocaml_name port.name
      ; default_type = "Hardcaml.Logic.Bit_vector.t"
      ; default_value = "Hardcaml.Logic.Bit_vector.of_string " ^ value
      ; bits
      ; constr = Hardcaml.Parameter.Value.Variants.bit_vector.name
      }
    in
    let create = if is_bitvector then create_bit_vector else create_std_logic_vector in
    let get_default = function
      | None -> Some ("default_vector", 0)
      | Some expr ->
        let rec f : Vhdl.Expression.t -> string option = function
          | String s -> Some s
          | Hex s ->
            let width = String.length s * 4 in
            Some
              (Hardcaml.Constant.of_hex_string ~signedness:Signed ~width s
               |> Hardcaml.Constant.to_binary_string)
          | Op ("&", a, b) -> Option.map2 (f a) (f b) ~f:( ^ )
          | _ -> None
        in
        Option.map (f expr) ~f:(fun s -> "\"" ^ s ^ "\"", String.length s)
    in
    let default_value, default_bits =
      Option.value (get_default port.default) ~default:("unknown_vector", 0)
    in
    match range with
    | None -> create (Int.to_string default_bits) default_value
    | Some (a, d, b) ->
      let hi, lo =
        match d with
        | Down -> a, b
        | Up -> b, a
      in
      create
        (sprintf
           "((%s) - (%s) + 1)"
           (expr hi ~id:ocaml_name_ref)
           (expr lo ~id:ocaml_name_ref))
        default_value
  ;;

  (* Perform VHDL to OCaml (adhoc) type inference *)
  let create ?(for_vhdl = false) (port : Vhdl.Port.t) ~is_generic =
    match port.type_ with
    | Std_logic | Bit -> std_logic port
    | Boolean -> boolean port ~for_vhdl
    | Real when is_generic -> real port
    | Integer when is_generic -> integer port
    | String when is_generic -> string port
    | Std_logic_vector range ->
      if is_generic
      then generic_vector port range ~is_bitvector:false
      else vector port range
    | Bit_vector range ->
      if is_generic
      then generic_vector port range ~is_bitvector:true
      else vector port range
    | _ -> not_implemented port
  ;;
end

let vhdl_component_to_ocaml_module
  ?for_vhdl
  ?(wrap_in_module = true)
  (comp : Vhdl.Component.t)
  =
  let generics =
    List.map comp.generics ~f:(Inferred_port.create ?for_vhdl ~is_generic:true)
  in
  let lines = String.concat ~sep:"\n" in
  let default_types =
    lines
      (List.map generics ~f:(fun param ->
         sprintf "    val %s : %s" param.ocaml_name param.default_type))
  in
  let default_values =
    lines
      (List.map generics ~f:(fun param ->
         sprintf "    let %s = %s" param.ocaml_name param.default_value))
  in
  let ocaml_params =
    lines
      (List.map generics ~f:(fun param ->
         sprintf
           "      Hardcaml.Parameter.create ~name:\"%s\" ~value:(%s P.%s);"
           param.port.name
           param.constr
           param.ocaml_name))
  in
  let with_module body =
    if wrap_in_module
    then
      String.concat
        [ "module "; String.capitalize comp.name; " = struct\n"; body; "end\n\n" ]
    else body
  in
  let header =
    String.concat
      [ "  module type P = sig\n"
      ; default_types
      ; "\n  end\n  module P : P = struct\n"
      ; default_values
      ; "\n  end\n  module Make (P : P) = struct\n    let params = [\n"
      ; ocaml_params
      ; "\n    ]\n"
      ]
  in
  let interface mod_name dir =
    let ports =
      List.filter comp.ports ~f:(fun port -> Vhdl.Port_direction.equal port.dir dir)
    in
    let ports = List.map ports ~f:(Inferred_port.create ~is_generic:false) in
    match ports with
    | [] -> String.concat [ "    module "; mod_name; " = Hardcaml.Interface.Empty\n" ]
    | _ ->
      let ocaml_ports =
        lines
          (List.map ports ~f:(fun port ->
             sprintf
               "        %s : 'a[@bits %s][@rtlname \"%s\"];"
               port.ocaml_name
               port.bits
               port.port.name))
      in
      String.concat
        [ "\n    module "
        ; mod_name
        ; " = struct\n      type 'a t = {\n"
        ; ocaml_ports
        ; "\n      }[@@deriving hardcaml ~rtlmangle:false]\n    end\n"
        ]
  in
  String.concat
    [ header
    ; interface "I" In
    ; interface "O" Out
    ; interface "T" Inout
    ; "\n\
      \    open struct include Hardcaml.Instantiation.With_interface(I)(O) end\n\
      \    let create ?lib ?arch ?attributes ?instance ?(name=\""
    ; comp.name
    ; "\") ?parameters inputs =\n\
      \      let parameters = Option.value ~default:params parameters in\n\
      \      create ?lib ?arch ?instance ?attributes ~parameters ~name inputs\n\
      \  end\n"
    ]
  |> with_module
;;
