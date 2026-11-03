(** External I/O delay relative to the rising edge of a project clock. Only maximum
    (setup) delay is specified; minimum (hold) delay is left unspecified. *)

open! Import

type t =
  { clock_name : string
  ; max_delay_ns : float
  (** External delay in nanoseconds, not a limit on the internal path delay. May be
      negative, but must be finite. *)
  }
[@@deriving sexp_of]
