(** Reading of report files generated by the synthesis project. *)

open! Import

module Subgroup : sig
  type t =
    { name : string
    ; value : int
    }
  [@@deriving sexp_of]
end

module Group : sig
  type t =
    { name : string
    ; value : int
    ; subgroups : Subgroup.t list
    }
  [@@deriving sexp_of]
end

module Clock : sig
  type t =
    { name : string
    ; setup : float
    ; hold : float
    }
  [@@deriving sexp_of]
end

(** Utilization and timing report data. *)
type t =
  { groups : Group.t list
  ; clocks : Clock.t list
  }
[@@deriving sexp_of]

(** Read back a report file generated by the vivado project tcl script and convert to a
    tree of groups and subgroups.   Includes the total module setup and hold slack. *)
val read : file_name:string -> t

(** Prints the utilization table hierarchically. *)
val print_utilization_table
  :  file:Out_channel.t
  -> top_level_name:string
  -> circuits:Circuit.t list
  -> (string * t option) list
  -> unit

(** Prints the timing table hierarchically. *)
val print_timing_table
  :  file:Out_channel.t
  -> top_level_name:string
  -> circuits:Circuit.t list
  -> (string * t option) list
  -> unit
