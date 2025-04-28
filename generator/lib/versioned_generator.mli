(** Given a set of Vivado versions, analyse all the component definitions contained in the
    unisim or xpm packages, and find those that are shared across versions. We then write
    out a minimal set of components.

    A top level module is written containing all the components for all versions, but
    using the shared components.

    All the supported versions are built into the library in the [Data] module. *)

module Package : sig
  type t =
    | Unisim
    | Xpm
  [@@deriving sexp_of]
end

val cinaps : Package.t -> unit
val command : Core.Command.t
