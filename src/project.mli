(** Vivado synthesis project generation. *)

open! Import

(** Project configuration. *)
module Config : sig
  type t =
    { vivado_utilization_report : bool (** Generate a Vivado utilization report file *)
    ; vivado_timing_report : bool (** Generate a Vivado timing report file *)
    ; primitive_groups : Primitive_group.t list
    (** Configure design utilization elements that should be queries and reported. *)
    ; opt_design : bool option
    (** Enable or disable the [opt_design] pass. If [None] then enabled automatically
        depending on the blackbox mode. *)
    ; report_hierarchy : bool
    (** When extracting cells from the design to report utilization, search complete
        hierarchy if true, or just the top-level (current) instance when false. Must be
        set to true to find XPM (Xilinx parameterized macro) instances. *)
    ; retiming : bool
    (** Enable retiming during synthesis. Improves timing but might affect resource usage.
        Enabled by default. *)
    ; additional_synth_args : string option
    (** Extra Tcl arguments to [synth_design]. Retiming and hierarchy controls still
        apply; [-top] and [-mode out_of_context] are appended. *)
    }
  [@@deriving sexp_of]

  val default : t
end

type t

module Hardcaml : sig
  module Config : sig
    type t =
      { common : Config.t
      ; full_design_hierarchy : bool (** Control how the design hierarchy is created. *)
      }
    [@@deriving sexp_of]

    val default : t
  end

  (** Create a Vivado project. This consists of the RTL generated for the given
      [Circuit.t] and project files. The function should be passed a list of top level
      clocks which specify the port name and requested frequency. [part_name] is the full
      FPGA part name including package a speed-grade. Files will be written to
      [output_path]. Performs synthesis by default but will also optionally run placement
      and routing. *)
  val create
    :  ?database:Circuit_database.t
    -> ?config:Config.t
    -> ?place:bool
    -> ?route:bool
    -> ?checkpoint:bool
    -> ?io_delays:Io_delay.t Core.String.Map.t
    -> clocks:Clock.t list
    -> part_name:string
    -> output_path:string
    -> preserve_hierarchy:bool
    -> Circuit.t
    -> t
end

module Verilog : sig
  val create
    :  ?config:Config.t
    -> ?place:bool
    -> ?route:bool
    -> ?checkpoint:bool
    -> clocks:Clock.t list
    -> part_name:string
    -> output_path:string
    -> preserve_hierarchy:bool
    -> verilog_file:string
    -> top_name:string
    -> unit
    -> t
end

(** Execute a project generated with [create].

    Uses [Unix.system] to run vivado in batch mode. *)
val run : ?verbose:bool -> ?path_to_vivado:string -> t -> Report.t option Async.Deferred.t

(** Output path where reports, artifacts and verilog files for this project lives. *)
val output_path : t -> string
