open Core
open Stdio

(* Only include components which match one of the regular expressions. *)
let filter_by_component_name regexps (component : Xilinx_vhdl.Vhdl.Component.t) =
  let name = component.name in
  let matches arg re =
    match arg with
    | None -> Re.exec_opt re name |> Option.map ~f:(fun _ -> component)
    | Some c -> Some c
  in
  List.fold regexps ~init:None ~f:matches
;;

let parse regexps vhdl =
  Xilinx_vhdl.Parse.parse_string vhdl
  |> Or_error.ok_exn
  |> List.filter_map ~f:(filter_by_component_name regexps)
  |> List.iter ~f:(fun component ->
    component
    |> Xilinx_vhdl.Vhdl_to_hardcaml.vhdl_component_to_ocaml_module
    |> Out_channel.output_string Out_channel.stdout)
;;

let list_by_regexp regexps vhdl =
  Xilinx_vhdl.Parse.parse_string vhdl
  |> Or_error.ok_exn
  |> List.filter_map ~f:(filter_by_component_name regexps)
  |> List.iter ~f:(fun component -> Stdio.printf "%s\n" component.name)
;;

(* Read from stdio, a file crunched into the [Data] module, or from the filesystem. *)
let get_vhdl filename =
  match filename with
  | None -> In_channel.input_all Stdio.stdin
  | Some filename ->
    (match Data.read filename with
     | Some file -> file
     | None -> In_channel.read_all filename)
;;

let get_filters filters =
  let compile re = Re.Posix.compile (Re.Posix.re re) in
  match filters with
  | [] -> [ compile ".*" ]
  | filters -> List.map filters ~f:compile
;;

include struct
  open Command.Spec

  let filters = flag "-filter" (listed string) ~doc:"REGEX Match module names"
  let vhdl = flag "-vhdl" (optional string) ~doc:"FILE VHDL component definitions"
end

let command_generator =
  Command.basic
    ~summary:"Generate ocaml module"
    [%map_open.Command
      let filters = filters
      and vhdl = vhdl in
      fun () ->
        let filters = get_filters filters in
        let vhdl = get_vhdl vhdl in
        parse filters vhdl]
;;

let command_list_modules =
  Command.basic
    ~summary:"List modules"
    [%map_open.Command
      let filters = filters
      and vhdl = vhdl in
      fun () ->
        let filters = get_filters filters in
        let vhdl = get_vhdl vhdl in
        list_by_regexp filters vhdl]
;;

let commands =
  Command.group
    ~summary:"Parse Xilinx VHDL components declarations"
    [ "generate", command_generator; "list", command_list_modules ]
;;

let () = Command_unix.run commands
