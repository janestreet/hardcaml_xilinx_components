open! Import

module Range_direction = struct
  type t =
    | Up
    | Down
  [@@deriving compare ~localize, sexp_of]

  let equal = [%compare.equal: t]
end

module Expression = struct
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
  [@@deriving compare ~localize, sexp_of]
end

module Range = struct
  type t = Expression.t * Range_direction.t * Expression.t
  [@@deriving compare ~localize, sexp_of]
end

module Type = struct
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
  [@@deriving compare ~localize, sexp_of]
end

module Port_direction = struct
  type t =
    | In
    | Inout
    | Out
  [@@deriving compare ~localize, enumerate, sexp_of]

  let equal = [%compare.equal: t]
  let to_string t = t |> [%sexp_of: t] |> Sexp.to_string
end

module Port = struct
  type t =
    { name : string
    ; dir : Port_direction.t
    ; type_ : Type.t
    ; default : Expression.t option
    }
  [@@deriving compare ~localize, sexp_of]
end

module Component = struct
  type t =
    { name : string
    ; generics : Port.t list
    ; ports : Port.t list
    }
  [@@deriving compare ~localize, sexp_of]
end
