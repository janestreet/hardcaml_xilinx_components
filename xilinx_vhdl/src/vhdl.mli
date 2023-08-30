open! Import

module Range_direction : sig
  type t =
    | Up
    | Down
  [@@deriving compare, sexp_of]

  include Equal.S with type t := t
end

module Expression : sig
  type t =
    | Char of string
    | Float of float
    | Hex of string
    | Id of string
    | Int of int
    | Neg of t
    | Op of string * t * t
    | Others of string
    | String of string
    | X01 of string
  [@@deriving compare, sexp_of]
end

module Range : sig
  type t = Expression.t * Range_direction.t * Expression.t [@@deriving compare, sexp_of]
end

module Type : sig
  type t =
    | Bit
    | Bit_vector of Range.t option
    | Boolean
    | Integer
    | Real
    | Std_logic
    | Std_logic_vector of Range.t option
    | String
    | Time
  [@@deriving compare, sexp_of]
end

module Port_direction : sig
  type t =
    | In
    | Inout
    | Out
  [@@deriving compare, enumerate, sexp_of]

  include Equal.S with type t := t

  val to_string : t -> string
end

module Port : sig
  type t =
    { name : string
    ; dir : Port_direction.t
    ; type_ : Type.t
    ; default : Expression.t option
    }
  [@@deriving compare, sexp_of]
end

module Component : sig
  type t =
    { name : string
    ; generics : Port.t list
    ; ports : Port.t list
    }
  [@@deriving compare, sexp_of]
end
