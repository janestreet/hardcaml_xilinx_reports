module Clock0 = Clock
open! Import
open! Core
open! Async
module Clock = Clock0

module Command_flags = struct
  open Command.Param

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
    ; verbose : bool
    ; path_to_vivado : string option
    ; max_concurrent_jobs : int option
    ; additional_output_log_files : string list
    ; preserve_hierarchy : bool
    }

  let default_flags =
    { output_path = ""
    ; part_name = ""
    ; reports = false
    ; clocks = []
    ; run = false
    ; place = false
    ; route = false
    ; checkpoint = false
    ; opt_design = None
    ; disable_hierarchy_in_report = false
    ; disable_retiming = false
    ; additional_synth_args = None
    ; verbose = false
    ; path_to_vivado = None
    ; max_concurrent_jobs = None
    ; additional_output_log_files = []
    ; preserve_hierarchy = false
    }
  ;;

  (* Specify clock parameters. We require a name, frequency and optional clock source
     location ie. "clock:230". *)
  let clock_arg =
    Command.Arg_type.create (fun s ->
      match String.split ~on:':' s |> List.filter ~f:(Fn.non String.is_empty) with
      | [ name; freq ] -> Clock.create_mhz ~name ~frequency_mhz:(Float.of_string freq) ()
      | [ name; freq; clk_src_bufg ] ->
        Clock.create_mhz ~clk_src_bufg ~name ~frequency_mhz:(Float.of_string freq) ()
      | clock_spec ->
        raise_s
          [%message
            "Invalid clock specification - expecting 'name:freq_mhz[:loc]'"
              (clock_spec : string list)])
  ;;

  (* Output path to write generated files, including the TCL project script. *)
  let output_path = flag "-dir" (required string) ~doc:"<DIR> build directory"

  (* Full FPGA part name ie 'xcvu9p-flbg-2104-2-e' *)
  let part_name = flag "-part" (required string) ~doc:"<PART> FPGA part name"
  let reports = flag "-reports" no_arg ~doc:" generate standard Vivado reports"

  (* Clocks. May be specified multiple times. *)
  let clocks = flag "-clock" (listed clock_arg) ~doc:"<NAME:FREQ[:LOC]> specific clock(s)"

  (* Run placement. *)
  let place =
    flag
      "-place"
      no_arg
      ~doc:" After synthesis has finished, run placement. Off by default."
  ;;

  (* Run routing. *)
  let route =
    flag
      "-route"
      no_arg
      ~doc:
        " After placement has finished, run routing. Ignored if -place is not used. Off \
         by default."
  ;;

  (* This dumps the checkpoint after each stage performed (synthesis, placement, and
     routing). *)
  let checkpoint =
    flag
      "-checkpoint"
      no_arg
      ~doc:" Enables dumping checkpoints after each stage (synth / place / route)"
  ;;

  (* Execute vivado *)
  let run = flag "-run" no_arg ~doc:" Run vivado"

  (* Run the [opt_design] pass, or not. If unspecified it is run depending on if the full
     design hierarchy is included. *)
  let opt_design = flag "-opt" (optional bool) ~doc:"<bool> run opt_design pass"

  let disable_hierarchy_in_report =
    flag
      "-disable-hierarchy-in-report"
      no_arg
      ~doc:" Dont scan hierarchy in utilization report"
  ;;

  (* Disable retiming during synthesis. *)
  let disable_retiming =
    flag "-disable-retiming" no_arg ~doc:" disables retiming during synthesis"
  ;;

  (* Dont hide vivado output. *)
  let verbose = flag "-verbose" no_arg ~doc:" Be verbose"

  let path_to_vivado =
    flag
      "-path-to-vivado"
      (optional string)
      ~doc:" Override path to vivado (defaults to vivado)"
  ;;

  let max_concurrent_jobs =
    flag "-jobs" (optional int) ~doc:" Max number of concurrent jobs (default: 1)"
  ;;

  let additional_output_log_files =
    flag
      "-output-log-files"
      (listed string)
      ~doc:" Additional paths to specify output log files on top of stdout."
  ;;

  let preserve_hierarchy =
    flag "-dont-flatten-hierarchy" no_arg ~doc:"Direct vivado to preserve hierarchy"
  ;;

  let flags ?(clocks = clocks) ?(part_name = part_name) () =
    let open Command.Let_syntax in
    let%map_open () = return ()
    and output_path
    and part_name
    and reports
    and clocks
    and run
    and place
    and route
    and checkpoint
    and opt_design
    and disable_hierarchy_in_report
    and disable_retiming
    and verbose
    and path_to_vivado
    and max_concurrent_jobs
    and additional_output_log_files
    and preserve_hierarchy in
    { output_path
    ; part_name
    ; reports
    ; clocks
    ; run
    ; place
    ; route
    ; checkpoint
    ; opt_design
    ; disable_hierarchy_in_report
    ; disable_retiming
    ; additional_synth_args = None
    ; verbose
    ; path_to_vivado
    ; max_concurrent_jobs
    ; additional_output_log_files
    ; preserve_hierarchy
    }
  ;;
end

module Hardcaml_command_flags = struct
  open Command.Param

  type t =
    { common : Command_flags.t
    ; full_design_hierarchy : bool
    ; flatten_design : bool
    ; hierarchy : bool
    }

  let default_flags =
    { common = Command_flags.default_flags
    ; full_design_hierarchy = false
    ; flatten_design = false
    ; hierarchy = false
    }
  ;;

  (* If true the top level circuit and all submodules are included. Otherwise submodules
     are created as blackboxes. *)
  let full_design_hierarchy =
    flag
      "-full-design-hierarchy"
      (optional_with_default default_flags.full_design_hierarchy bool)
      ~doc:" Instantiate submodules or leave as blackboxes."
  ;;

  (* Flatten design during elaboration. *)
  let flatten_design =
    flag "-flatten" no_arg ~doc:" flatten design hierarchy during elaboration"
  ;;

  (* Perform synthesis on the top level module, and all children. May take a while... *)
  let hierarchy = flag "-hierarchy" no_arg ~doc:" synthesize full design hierarchy"

  let flags ?clocks ?part_name ?(full_design_hierarchy = full_design_hierarchy) () =
    let open Command.Let_syntax in
    let%map_open common = Command_flags.flags ?clocks ?part_name ()
    and full_design_hierarchy
    and flatten_design
    and hierarchy in
    { common; full_design_hierarchy; flatten_design; hierarchy }
  ;;
end

let default_primitive_groups =
  Primitive_group.
    [ Clb [ Lut; Muxf; Carry; Lutram; Srl ]
    ; Register [ Sdr ]
    ; Blockram [ Fifo; Bram; Uram ]
    ]
;;

let hierarchical_projects
  ~top_level_name
  ~io_delays
  ~database
  ~primitive_groups
  ~output_path
  ~part_name
  ~reports
  ~full_design_hierarchy
  ~opt_design
  ~clocks
  ~disable_hierarchy_in_report
  ~disable_retiming
  ~additional_synth_args
  ~place
  ~route
  ~checkpoint
  ~preserve_hierarchy
  circuits
  =
  let create_project circuit =
    let input_names =
      List.concat_map (Circuit.inputs circuit) ~f:Hardcaml.Signal.names
      @ List.map (Circuit.phantom_inputs circuit) ~f:fst
    in
    let clocks =
      List.filter clocks ~f:(fun clock ->
        List.mem input_names (Clock.name clock) ~equal:String.equal)
    in
    let top_name = Circuit.name circuit in
    let io_delays =
      (* External timing describes the top-level boundary, not similarly named ports of
         independently synthesized child circuits. *)
      if String.equal top_name top_level_name then io_delays else String.Map.empty
    in
    let%map () = Unix.mkdir ~p:() (output_path ^/ top_name) in
    Project.Hardcaml.create
      ~io_delays
      ~database
      ~clocks
      ~config:
        { common =
            { additional_synth_args
            ; vivado_utilization_report = reports
            ; vivado_timing_report = reports
            ; primitive_groups
            ; opt_design
            ; report_hierarchy = not disable_hierarchy_in_report
            ; retiming = not disable_retiming
            }
        ; full_design_hierarchy
        }
      ~output_path:(output_path ^/ top_name)
      ~part_name
      ~place
      ~route
      ~checkpoint
      ~preserve_hierarchy
      circuit
  in
  printf "Creating xilinx report projects in %s\n" output_path;
  Deferred.List.map ~how:`Sequential circuits ~f:(fun circuit ->
    let name = Circuit.name circuit in
    let%bind project = create_project circuit in
    Deferred.return (name, project))
;;

let run_and_print
  ?(sort_by_name = false)
  ?path_to_vivado
  ~max_concurrent_jobs
  ~verbose
  ~additional_output_log_files
  ~additional_reports
  projects
  =
  let%map runs =
    Deferred.List.map
      ~how:
        (match max_concurrent_jobs with
         | None -> `Max_concurrent_jobs 8
         | Some x -> `Max_concurrent_jobs x)
      projects
      ~f:(fun (name, project) ->
        Stdio.printf "Running project for %s\n" name;
        let%bind results = Project.run ?path_to_vivado ~verbose project in
        Stdio.printf "Completed project for %s\n" name;
        return (name, results))
  in
  let runs =
    if sort_by_name
    then List.sort runs ~compare:(fun (k1, _) (k2, _) -> String.compare k1 k2)
    else runs
  in
  let additional_output_log_files =
    List.map additional_output_log_files ~f:(fun fname -> Out_channel.create fname)
  in
  List.iter (Out_channel.stdout :: additional_output_log_files) ~f:(fun file ->
    List.iter projects ~f:(fun (name, project) ->
      Out_channel.fprintf
        file
        "Project for %s lives in %s\n"
        name
        (Project.output_path project));
    List.iter additional_reports ~f:(fun report_fn -> report_fn ~file runs));
  List.iter additional_output_log_files ~f:(fun oc -> Out_channel.close oc)
;;

let run_verilog
  ?(primitive_groups = default_primitive_groups)
  ~name
  ~verilog_file
  (flags : Command_flags.t)
  =
  let { Command_flags.output_path
      ; part_name
      ; reports
      ; clocks
      ; run
      ; place
      ; route
      ; checkpoint
      ; opt_design
      ; disable_hierarchy_in_report
      ; disable_retiming
      ; additional_synth_args
      ; verbose
      ; path_to_vivado
      ; max_concurrent_jobs
      ; additional_output_log_files
      ; preserve_hierarchy
      }
    =
    flags
  in
  let top_level_name = name in
  let project =
    Project.Verilog.create
      ~config:
        { vivado_utilization_report = reports
        ; vivado_timing_report = reports
        ; primitive_groups
        ; opt_design
        ; report_hierarchy = not disable_hierarchy_in_report
        ; retiming = not disable_retiming
        ; additional_synth_args
        }
      ~place
      ~route
      ~checkpoint
      ~clocks
      ~part_name
      ~output_path
      ~preserve_hierarchy
      ~verilog_file
      ~top_name:top_level_name
      ()
  in
  if run
  then
    run_and_print
      ?path_to_vivado
      ~max_concurrent_jobs
      ~verbose
      ~additional_output_log_files
      ~additional_reports:
        [ Report.print_utilization_table ~top_level_name
        ; Report.print_timing_table ~top_level_name
        ]
      [ top_level_name, project ]
  else Deferred.return ()
;;

let hierarchical_run_and_print
  ?sort_by_name
  ?path_to_vivado
  ~top_level_name
  ~circuits
  ~max_concurrent_jobs
  ~verbose
  ~additional_output_log_files
  projects
  =
  run_and_print
    ?sort_by_name
    ?path_to_vivado
    ~max_concurrent_jobs
    ~verbose
    ~additional_output_log_files
    ~additional_reports:
      [ Report.print_hierarchical_utilization_table ~top_level_name ~circuits
      ; Report.print_hierarchical_timing_table ~top_level_name ~circuits
      ]
    projects
;;

let run_circuit
  ?(io_delays = String.Map.empty)
  ?(primitive_groups = default_primitive_groups)
  ?sort_by_name
  ~(flags : Hardcaml_command_flags.t)
  circuit
  =
  let { Hardcaml_command_flags.common =
          { output_path
          ; part_name
          ; reports
          ; clocks
          ; run
          ; place
          ; route
          ; checkpoint
          ; opt_design
          ; disable_hierarchy_in_report
          ; disable_retiming
          ; additional_synth_args
          ; verbose
          ; path_to_vivado
          ; max_concurrent_jobs
          ; additional_output_log_files
          ; preserve_hierarchy
          }
      ; full_design_hierarchy
      ; flatten_design
      ; hierarchy
      }
    =
    flags
  in
  if run
  then (
    let scope = Scope.create ~flatten_design () in
    let circuit = circuit scope in
    let database = Scope.circuit_database scope in
    let circuits =
      if hierarchy then circuit :: Circuit_database.get_circuits database else [ circuit ]
    in
    let%bind projects =
      hierarchical_projects
        ~top_level_name:(Hardcaml.Circuit.name circuit)
        ~io_delays
        ~database
        ~primitive_groups
        ~output_path
        ~part_name
        ~reports
        ~full_design_hierarchy
        ~opt_design
        ~clocks
        ~disable_hierarchy_in_report
        ~disable_retiming
        ~additional_synth_args
        ~place
        ~route
        ~checkpoint
        ~preserve_hierarchy
        circuits
    in
    if run
    then
      hierarchical_run_and_print
        ?path_to_vivado
        ?sort_by_name
        ~top_level_name:(Circuit.name circuit)
        ~circuits
        ~verbose
        ~max_concurrent_jobs
        ~additional_output_log_files
        projects
    else Deferred.unit)
  else Deferred.unit
;;

let command_circuit ?io_delays ?primitive_groups ?sort_by_name circuit =
  Command.async
    ~summary:"Circuit Synthesis"
    [%map_open.Command
      let flags = Hardcaml_command_flags.flags () in
      fun () -> run_circuit ?io_delays ?primitive_groups ?sort_by_name ~flags circuit]
;;

module With_interface (I : Interface.S) (O : Interface.S) = struct
  module Circuit = Circuit.With_interface (I) (O)

  let run
    ?(input_delays = I.const None)
    ?(output_delays = O.const None)
    ?(primitive_groups = default_primitive_groups)
    ?sort_by_name
    ~name
    ~flags
    create
    =
    let { Hardcaml_command_flags.common =
            { output_path
            ; part_name
            ; reports
            ; clocks
            ; run
            ; place
            ; route
            ; checkpoint
            ; opt_design
            ; disable_hierarchy_in_report
            ; disable_retiming
            ; additional_synth_args
            ; verbose
            ; path_to_vivado
            ; max_concurrent_jobs
            ; additional_output_log_files
            ; preserve_hierarchy
            }
        ; full_design_hierarchy
        ; flatten_design
        ; hierarchy
        }
      =
      flags
    in
    let io_delays =
      I.to_list (I.zip I.port_names input_delays)
      @ O.to_list (O.zip O.port_names output_delays)
      |> List.filter_map ~f:(fun (name, delay) ->
        Option.map delay ~f:(fun delay -> name, delay))
      |> String.Map.of_alist_exn
    in
    let scope = Scope.create ~flatten_design () in
    let circuit = Circuit.create_exn ~name (create scope) in
    let database = Scope.circuit_database scope in
    let circuits =
      if hierarchy then circuit :: Circuit_database.get_circuits database else [ circuit ]
    in
    let%bind projects =
      hierarchical_projects
        ~top_level_name:(Hardcaml.Circuit.name circuit)
        ~io_delays
        ~database
        ~primitive_groups
        ~output_path
        ~part_name
        ~reports
        ~full_design_hierarchy
        ~opt_design
        ~clocks
        ~disable_hierarchy_in_report
        ~disable_retiming
        ~additional_synth_args
        ~place
        ~route
        ~checkpoint
        ~preserve_hierarchy
        circuits
    in
    if run
    then
      hierarchical_run_and_print
        ?path_to_vivado
        ?sort_by_name
        ~top_level_name:(Hardcaml.Circuit.name circuit)
        ~circuits
        ~max_concurrent_jobs
        ~verbose
        ~additional_output_log_files
        projects
    else Deferred.unit
  ;;

  let command_basic
    ?input_delays
    ?output_delays
    ?primitive_groups
    ?sort_by_name
    ~name
    create
    =
    Command.async
      ~summary:("Synthesis reports for " ^ name)
      (let open Command.Let_syntax in
       let%map_open () = return ()
       and flags = Hardcaml_command_flags.flags () in
       fun () ->
         run
           ?input_delays
           ?output_delays
           ?sort_by_name
           ?primitive_groups
           ~name
           ~flags
           create)
  ;;

  let command_run
    ?input_delays
    ?output_delays
    ?primitive_groups
    ?sort_by_name
    ~name
    create
    =
    Command_unix.run
      ~version:"0.0"
      (command_basic
         ?input_delays
         ?output_delays
         ?sort_by_name
         ?primitive_groups
         ~name
         create)
  ;;
end
