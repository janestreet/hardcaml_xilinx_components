open Core

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

let write ~split_into_files (component : Xilinx_vhdl.Vhdl.Component.t) modl =
  if not split_into_files
  then Out_channel.output_string Out_channel.stdout modl
  else
    Out_channel.with_file
      (String.capitalize component.name ^ ".ml")
      ~f:(fun file -> Out_channel.output_string file modl)
;;

let parse ~for_vhdl ~split_into_files regexps vhdl =
  Xilinx_vhdl.Parse.parse_string vhdl
  |> Or_error.ok_exn
  |> List.filter_map ~f:(filter_by_component_name regexps)
  |> List.iter ~f:(fun component ->
    let modl =
      Xilinx_vhdl.Vhdl_to_hardcaml.vhdl_component_to_ocaml_module
        ~for_vhdl
        ~wrap_in_module:(not split_into_files)
        component
    in
    write ~split_into_files component modl)
;;

let list_by_regexp regexps vhdl =
  Xilinx_vhdl.Parse.parse_string vhdl
  |> Or_error.ok_exn
  |> List.filter_map ~f:(filter_by_component_name regexps)
  |> List.iter ~f:(fun component -> printf "%s\n" component.name)
;;

(* Read from stdio, a file crunched into the [Data] module, or from the filesystem. *)
let get_vhdl filename =
  match filename with
  | None -> In_channel.input_all In_channel.stdin
  | Some filename ->
    let filename =
      match String.split filename ~on:'-' with
      | [ "xpm"; ver ] -> [%string "xpm_VCOMP_%{ver}.vhd"]
      | [ "unisim"; ver ] -> [%string "unisim_VCOMP_%{ver}.vhd"]
      | _ -> raise_s [%message "Can't find VCOMP file" (filename : string)]
    in
    (match List.Assoc.find Data.by_filename filename ~equal:String.equal with
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
    ~summary:"Generate OCaml module"
    [%map_open.Command
      let filters = filters
      and vhdl = vhdl
      and for_vhdl =
        flag "-for-vhdl" no_arg ~doc:"Generate code suitable for writing back to VHDL"
      and split_into_files =
        flag "-split-into-files" no_arg ~doc:"Write a seperate file for each component"
      in
      fun () ->
        let filters = get_filters filters in
        let vhdl = get_vhdl vhdl in
        parse ~for_vhdl ~split_into_files filters vhdl]
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
