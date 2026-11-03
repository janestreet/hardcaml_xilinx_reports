(** Generate a generic command line for performing synthesis runs over a given design. *)

open! Import

module Command_flags : sig
  type t =
    { output_path : string
    ; part_name : string
    ; reports : bool
    ; clocks : Clock.t list
    ; run : bool
    ; place : bool
    ; route : bool
    ; checkpoint : bool
    ; opt_design : bool option
    ; disable_hierarchy_in_report : bool
    ; disable_retiming : bool
    ; additional_synth_args : string option
    (** Programmatic supplement to [synth_design] arguments. [flags] initializes this to
        [None]; callers can update it after parsing the command line. *)
    ; verbose : bool
    ; path_to_vivado : string option
    ; max_concurrent_jobs : int option
    ; additional_output_log_files : string list
    ; preserve_hierarchy : bool
    (** If [true], use [-flatten_hierarchy none] in [synth_design] *)
    }

  val default_flags : t

  val flags
    :  ?clocks:Clock.t list Core.Command.Param.t
    -> ?part_name:string Core.Command.Param.t
    -> unit
    -> t Core.Command.Param.t
end

module Hardcaml_command_flags : sig
  type t =
    { common : Command_flags.t
    ; full_design_hierarchy : bool
    ; flatten_design : bool
    ; hierarchy : bool
    }

  val default_flags : t

  val flags
    :  ?clocks:Clock.t list Core.Command.Param.t
    -> ?part_name:string Core.Command.Param.t
    -> ?full_design_hierarchy:bool Core.Command.Param.t
    -> unit
    -> t Core.Command.Param.t
end

val run_verilog
  :  ?primitive_groups:Primitive_group.t list
  -> name:string
  -> verilog_file:string
  -> Command_flags.t
  -> unit Async.Deferred.t

val command_circuit
  :  ?io_delays:Io_delay.t Core.String.Map.t
  -> ?primitive_groups:Primitive_group.t list
  -> ?sort_by_name:bool
  -> (Scope.t -> Circuit.t)
  -> Core.Command.t

val run_circuit
  :  ?io_delays:Io_delay.t Core.String.Map.t
  -> ?primitive_groups:Primitive_group.t list
  -> ?sort_by_name:bool
  -> flags:Hardcaml_command_flags.t
  -> (Scope.t -> Circuit.t)
  -> unit Async.Deferred.t

(** I/O delays apply only to the top-level circuit, including when synthesizing the
    hierarchy. [None] leaves a port unconstrained; a bus delay applies to every bit. *)
module With_interface (I : Interface.S) (O : Interface.S) : sig
  val run
    :  ?input_delays:Io_delay.t option I.t
    -> ?output_delays:Io_delay.t option O.t
    -> ?primitive_groups:Primitive_group.t list
    -> ?sort_by_name:bool
    -> name:string
    -> flags:Hardcaml_command_flags.t
    -> (Scope.t -> Interface.Create_fn(I)(O).t)
    -> unit Async.Deferred.t

  val command_basic
    :  ?input_delays:Io_delay.t option I.t
    -> ?output_delays:Io_delay.t option O.t
    -> ?primitive_groups:Primitive_group.t list
    -> ?sort_by_name:bool
    -> name:string
    -> (Scope.t -> Interface.Create_fn(I)(O).t)
    -> Core.Command.t

  val command_run
    :  ?input_delays:Io_delay.t option I.t
    -> ?output_delays:Io_delay.t option O.t
    -> ?primitive_groups:Primitive_group.t list
    -> ?sort_by_name:bool
    -> name:string
    -> (Scope.t -> Interface.Create_fn(I)(O).t)
    -> unit
end
