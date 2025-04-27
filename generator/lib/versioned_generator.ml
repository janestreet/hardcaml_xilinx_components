open Core

(* Optionally add names of modules here to exclude them from the build, this can sometimes
   help with long OCaml compile times. *)
let excluded_modules = []

module Package = struct
  type t =
    | Unisim
    | Xpm
  [@@deriving sexp_of]

  let to_string = function
    | Unisim -> "unisim"
    | Xpm -> "xpm"
  ;;

  let of_string = function
    | "unisim" -> Unisim
    | "xpm" -> Xpm
    | _ -> raise_s [%message "Invalid package anem"]
  ;;
end

module Versions : sig
  module V : sig
    type t [@@deriving sexp_of, compare]

    val to_string : t -> string
    val to_module_name : t -> string

    include Comparator.S with type t := t
  end

  type t [@@deriving sexp_of]

  val create : Package.t -> t
  val fold : t -> init:'a -> f:('a -> V.t -> 'a) -> 'a
  val iter : t -> f:(V.t -> unit) -> unit
end = struct
  module V = struct
    include String

    let to_string = Fn.id

    let to_module_name t =
      String.map (to_string t) ~f:(function
        | '.' -> '_'
        | c -> c)
    ;;
  end

  type t = V.t list [@@deriving sexp_of]

  (* Find the versions built into the tool from the [Data] module. *)
  let create package =
    let prefix = Package.to_string package in
    let names = Data.by_filename in
    let versions =
      List.filter_map names ~f:(fun (name, _) ->
        match String.split ~on:'_' name with
        | [ m; "VCOMP"; s ] when String.equal m prefix ->
          (match String.split ~on:'.' s with
           | [ v; s; "vhd" ] -> Some (v, s)
           | _ -> None)
        | _ -> None)
      |> List.sort ~compare:[%compare: string * string]
      |> List.map ~f:(fun (v, s) -> v ^ "." ^ s)
    in
    versions
  ;;

  let fold = List.fold
  let iter = List.iter
end

module Parse_tree = struct
  type t = Xilinx_vhdl.Vhdl.Component.t
end

module Parse_trees_of_version : sig
  type t

  (* Load the parse trees for the given version. *)
  val create : Package.t -> Versions.V.t -> t

  val fold
    :  t
    -> init:'a
    -> f:(component_name:string -> parse_tree:Parse_tree.t -> 'a -> 'a)
    -> 'a

  (* Find a parse tree for the specified component. *)
  val find : t -> component_name:string -> Parse_tree.t option
end = struct
  type t = Parse_tree.t Map.M(String).t

  let versioned_package_name package version =
    Package.to_string package ^ "_VCOMP_" ^ Versions.V.to_string version ^ ".vhd"
  ;;

  let create package version =
    let vhdl =
      List.Assoc.find_exn
        Data.by_filename
        (versioned_package_name package version)
        ~equal:String.equal
    in
    let components = Xilinx_vhdl.Parse.parse_string vhdl |> Or_error.ok_exn in
    List.fold
      components
      ~init:(Map.empty (module String))
      ~f:(fun map comp ->
        (* filter out excluded modules *)
        if List.mem excluded_modules comp.name ~equal:String.equal
        then map
        else Map.add_exn map ~key:comp.name ~data:comp)
  ;;

  let fold t ~init ~f =
    Map.fold t ~init ~f:(fun ~key ~data acc -> f ~component_name:key ~parse_tree:data acc)
  ;;

  let find t ~component_name = Map.find t component_name
end

module Parse_trees : sig
  type t

  (* Load parse trees for all versions. *)
  val create : Package.t -> Versions.t -> t

  (* The set of all components across all versions. *)
  val all_component_names : t -> string list

  val fold
    :  t
    -> init:'a
    -> f:(version:Versions.V.t -> parse_trees:Parse_trees_of_version.t -> 'a -> 'a)
    -> 'a

  (* Find parse tree for component at specified revision. *)
  val find : t -> component_name:string -> version:Versions.V.t -> Parse_tree.t option
end = struct
  type t = Parse_trees_of_version.t Map.M(Versions.V).t

  let create package versions =
    Versions.fold
      versions
      ~init:(Map.empty (module Versions.V))
      ~f:(fun map version ->
        Map.add_exn map ~key:version ~data:(Parse_trees_of_version.create package version))
  ;;

  let all_component_names parse_trees =
    Map.fold
      parse_trees
      ~init:(Set.empty (module String))
      ~f:(fun ~key:_ ~data:v set ->
        Parse_trees_of_version.fold
          v
          ~init:set
          ~f:(fun ~component_name ~parse_tree:_ set -> Set.add set component_name))
    |> Set.to_list
  ;;

  let fold t ~init ~f =
    Map.fold t ~init ~f:(fun ~key ~data acc -> f ~version:key ~parse_trees:data acc)
  ;;

  let find t ~component_name ~version =
    match Map.find t version with
    | None -> None
    | Some parse_trees -> Parse_trees_of_version.find parse_trees ~component_name
  ;;
end

module Versioned_component_mapping : sig
  type t = Versions.V.t Map.M(Versions.V).t [@@deriving sexp_of]

  (* Given a set of version parse trees, work out for each compoent the earliest equal
     revision. This means later versions with no changes will be pointed to an earlier
     revision. *)
  val create : Versions.t -> Parse_trees.t -> string -> t
end = struct
  type t = Versions.V.t Map.M(Versions.V).t [@@deriving sexp_of]

  let create versions parse_trees component_name =
    let parse_trees =
      Parse_trees.fold
        parse_trees
        ~init:(Map.empty (module Versions.V))
        ~f:(fun ~version ~parse_trees map ->
          match Parse_trees_of_version.find parse_trees ~component_name with
          | None -> map
          | Some tree -> Map.add_exn map ~key:version ~data:tree)
    in
    snd
    @@ Versions.fold
         versions
         ~init:(None, Map.empty (module Versions.V))
         ~f:(fun (prev_tree, map) version ->
           let cur_tree = Map.find parse_trees version in
           match prev_tree, cur_tree with
           | prev, None -> prev, map
           | None, Some cur_tree ->
             Some (version, cur_tree), Map.add_exn map ~key:version ~data:version
           | Some (prev_version, prev_tree), Some cur_tree ->
             if Xilinx_vhdl.Vhdl.Component.compare prev_tree cur_tree = 0
             then
               ( Some (prev_version, prev_tree)
               , Map.add_exn map ~key:version ~data:prev_version )
             else Some (version, cur_tree), Map.add_exn map ~key:version ~data:version)
  ;;
end

let versioned_components package =
  let versions = Versions.create package in
  let parse_trees = Parse_trees.create package versions in
  let all_component_names = Parse_trees.all_component_names parse_trees in
  let versioned =
    List.map all_component_names ~f:(fun component_name ->
      ( component_name
      , Versioned_component_mapping.create versions parse_trees component_name ))
  in
  versions, parse_trees, versioned
;;

(* All components and at their required versions. *)
let unique_components versioned =
  List.map versioned ~f:(fun (component_name, version_map) ->
    let versions =
      Map.data version_map |> Set.of_list (module Versions.V) |> Set.to_list
    in
    component_name, versions)
;;

let versioned_module_name component_name version =
  String.uppercase component_name ^ "_" ^ Versions.V.to_module_name version
;;

let versioned_file_name package =
  "hardcaml_xilinx_" ^ Package.to_string package ^ "_versioned.ml"
;;

(* Generate the cinaps rules defining the complete set of generated file targets. *)
let cinaps package =
  let _, _, versioned = versioned_components package in
  let unique_components = unique_components versioned in
  printf "(rule (\n";
  printf "  (targets (\n";
  printf "     %s\n" (versioned_file_name package);
  List.iter unique_components ~f:(fun (name, versions) ->
    List.iter versions ~f:(fun version ->
      let name = versioned_module_name name version in
      printf "    %s.ml\n" name));
  printf "  ))\n";
  printf
    "  (deps (%%{root}/lib/hardcaml/xilinx_components/generator/bin/generator.exe))\n";
  printf
    "  (action \"%%{first_dep} versioned generate-modules -package %s\")))\n"
    (Package.to_string package)
;;

let print_component_versions package component_name =
  let versions = Versions.create package in
  let parse_trees = Parse_trees.create package versions in
  let versioning =
    Versioned_component_mapping.create versions parse_trees component_name
  in
  print_s [%message (versioning : Versioned_component_mapping.t)]
;;

let write_component parse_trees component_name version =
  let module_name = versioned_module_name component_name version ^ ".ml" in
  let parse_tree = Parse_trees.find parse_trees ~component_name ~version in
  Option.iter parse_tree ~f:(fun parse_tree ->
    let modl =
      Xilinx_vhdl.Vhdl_to_hardcaml.vhdl_component_to_ocaml_module
        ~for_vhdl:false
        ~wrap_in_module:false
        parse_tree
    in
    Out_channel.with_file module_name ~f:(fun file -> Out_channel.output_string file modl))
;;

let write_versioned_interface package versions versioned =
  Out_channel.with_file (versioned_file_name package) ~f:(fun file ->
    Versions.iter versions ~f:(fun version ->
      fprintf file "module V%s = struct\n" (Versions.V.to_module_name version);
      List.iter versioned ~f:(fun (component_name, version_map) ->
        match Map.find version_map version with
        | None -> ()
        | Some to_version ->
          let name = String.uppercase component_name in
          fprintf
            file
            "  module %s = %s_%s\n"
            name
            name
            (Versions.V.to_module_name to_version));
      fprintf file "end\n\n"))
;;

let generate_modules package =
  let versions, parse_trees, versioned = versioned_components package in
  let unique_components = unique_components versioned in
  (* write out components at all the required revisions *)
  List.iter unique_components ~f:(fun (component_name, versions) ->
    List.iter versions ~f:(fun version ->
      write_component parse_trees component_name version));
  (* write the mapping for each version for all components at all versions *)
  write_versioned_interface package versions versioned
;;

let command_component_versions =
  Command.basic
    ~summary:""
    [%map_open.Command
      let package =
        flag
          "-package"
          (optional_with_default Package.Unisim (Arg_type.create Package.of_string))
          ~doc:""
      and component_name = flag "-component" (required string) ~doc:"" in
      fun () -> print_component_versions package component_name]
;;

let command_cinaps =
  Command.basic
    ~summary:""
    [%map_open.Command
      let package =
        flag
          "-package"
          (optional_with_default Package.Unisim (Arg_type.create Package.of_string))
          ~doc:""
      in
      fun () -> cinaps package]
;;

let command_generate_modules =
  Command.basic
    ~summary:""
    [%map_open.Command
      let package =
        flag
          "-package"
          (optional_with_default Package.Unisim (Arg_type.create Package.of_string))
          ~doc:""
      in
      fun () -> generate_modules package]
;;

let command =
  Command.group
    ~summary:""
    [ "component-versions", command_component_versions
    ; "cinaps", command_cinaps
    ; "generate-modules", command_generate_modules
    ]
;;
