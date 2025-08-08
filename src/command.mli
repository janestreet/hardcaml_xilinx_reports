(** Generate a generic command line for performing synthesis runs over a given design. *)

open! Import

module Command_flags : sig
  type t =
    { output_path : string
    ; part_name : string
    ; reports : bool
    ; full_design_hierarchy : bool
    ; clocks : Clock.t list
    ; run : bool
    ; place : bool
    ; route : bool
    ; checkpoint : bool
    ; opt_design : bool option
    ; flatten_design : bool
    ; hierarchy : bool
    ; disable_hierarchy_in_report : bool
    ; disable_retiming : bool
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
    -> ?full_design_hierarchy:bool Core.Command.Param.t
    -> unit
    -> t Core.Command.Param.t
end

val command_circuit
  :  ?primitive_groups:Primitive_group.t list
  -> ?sort_by_name:bool
  -> (Scope.t -> Circuit.t)
  -> Core.Command.t

val run_circuit
  :  ?primitive_groups:Primitive_group.t list
  -> ?sort_by_name:bool
  -> flags:Command_flags.t
  -> (Scope.t -> Circuit.t)
  -> unit Async.Deferred.t

module With_interface (I : Interface.S) (O : Interface.S) : sig
  val run
    :  ?primitive_groups:Primitive_group.t list
    -> ?sort_by_name:bool
    -> name:string
    -> flags:Command_flags.t
    -> (Scope.t -> Interface.Create_fn(I)(O).t)
    -> unit Async.Deferred.t

  val command_basic
    :  ?primitive_groups:Primitive_group.t list
    -> ?sort_by_name:bool
    -> name:string
    -> (Scope.t -> Interface.Create_fn(I)(O).t)
    -> Core.Command.t

  val command_run
    :  ?primitive_groups:Primitive_group.t list
    -> ?sort_by_name:bool
    -> name:string
    -> (Scope.t -> Interface.Create_fn(I)(O).t)
    -> unit
end
