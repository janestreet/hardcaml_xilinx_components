open! Import

val parse_in_channel : In_channel.t -> Vhdl.Component.t list Or_error.t
val parse_string     : string       -> Vhdl.Component.t list Or_error.t
