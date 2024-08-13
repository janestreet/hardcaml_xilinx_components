open! Import

(** Generate a module from a component declaration. *)
val vhdl_component_to_ocaml_module
  :  ?for_vhdl:bool
  -> ?wrap_in_module:bool
  -> Vhdl.Component.t
  -> string
