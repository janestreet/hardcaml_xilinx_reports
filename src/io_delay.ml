open! Import

type t =
  { clock_name : string
  ; max_delay_ns : float
  }
[@@deriving sexp_of]
