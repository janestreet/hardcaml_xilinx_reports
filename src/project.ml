open! Import

module Config = struct
  type t =
    { vivado_utilization_report : bool
    ; vivado_timing_report : bool
    ; primitive_groups : Primitive_group.t list
    ; opt_design : bool option
    ; report_hierarchy : bool
    ; retiming : bool
    ; additional_synth_args : string option
    }
  [@@deriving sexp_of]

  let default =
    { vivado_utilization_report = false
    ; vivado_timing_report = false
    ; primitive_groups = [ Clb []; Register [] ]
    ; opt_design = None
    ; report_hierarchy = true
    ; retiming = true
    ; additional_synth_args = None
    }
  ;;
end

type t =
  { tcl_file : File_path_and_name.t
  ; report_file : File_path_and_name.t
  ; output_path : string
  }

let tcl_quote s =
  let escaped =
    String.concat_map s ~f:(function
      | ('\\' | '"' | '$' | '[' | ']') as c -> "\\" ^ String.of_char c
      | '\n' -> "\\n"
      | '\r' -> "\\r"
      | c -> String.of_char c)
  in
  "\"" ^ escaped ^ "\""
;;

let write_xdc ~xdc_file ~clocks ~io_delays =
  Map.iteri
    io_delays
    ~f:(fun ~key:port ~data:(_, ({ clock_name; max_delay_ns } : Io_delay.t)) ->
      if not
           (List.exists clocks ~f:(fun clock ->
              String.equal (Clock.name clock) clock_name))
      then
        raise_s [%message "Unknown I/O delay clock" (port : string) (clock_name : string)];
      if List.exists clocks ~f:(fun clock -> String.equal (Clock.name clock) port)
      then raise_s [%message "Cannot set I/O delay on a clock port" (port : string)];
      if not (Float.is_finite max_delay_ns)
      then
        raise_s [%message "Non-finite I/O delay" (port : string) (max_delay_ns : float)]);
  let xdc_file = Out_channel.create (File_path_and_name.full_path_name xdc_file) in
  Out_channel.fprintf xdc_file "# Clock constraints\n";
  List.iter clocks ~f:(fun clock ->
    Out_channel.fprintf
      xdc_file
      "create_clock -name \"%s\" -period %.3f [get_ports \"%s\"]\n"
      (Clock.name clock)
      (Clock.period clock)
      (Clock.name clock);
    Option.iter (Clock.clk_src_bufg clock) ~f:(fun clk_src_bufg ->
      Out_channel.fprintf
        xdc_file
        "set_property HD.CLK_SRC %s [get_ports \"%s\"]\n"
        clk_src_bufg
        (Clock.name clock)));
  Map.iteri
    io_delays
    ~f:(fun ~key:port ~data:(direction, ({ clock_name; max_delay_ns } : Io_delay.t)) ->
      let direction =
        match direction with
        | `Input -> "input"
        | `Output -> "output"
      in
      (* [get_ports] expands a bus name to all its bits. *)
      Out_channel.fprintf
        xdc_file
        "set_%s_delay -clock [get_clocks %s] -max %s [get_ports %s]\n"
        direction
        (tcl_quote clock_name)
        (Float.to_string max_delay_ns)
        (tcl_quote port));
  Out_channel.close xdc_file
;;

let query_utilization ~(config : Config.t) ~tcl_file ~primitive_groups =
  let get_cells = if config.report_hierarchy then "get_cells -hier" else "get_cells" in
  List.iter primitive_groups ~f:(fun (primitive_group : Primitive_group.t) ->
    let primitive_group_name = Primitive_group.primitive_group primitive_group in
    Out_channel.fprintf
      tcl_file
      "set v_%s [llength [%s -filter {PRIMITIVE_GROUP == \"%s\"}]]\n"
      primitive_group_name
      get_cells
      primitive_group_name;
    Out_channel.fprintf
      tcl_file
      "puts $report_file \"GROUP %s $v_%s\"\n"
      primitive_group_name
      primitive_group_name;
    let write_subgroups
      (type t)
      (module X : Primitive_group.Subgroup with type t = t)
      (primitive_subgroups : t list)
      =
      List.iter primitive_subgroups ~f:(fun primitive_subgroup ->
        let primitive_subgroup_name = X.primitive_subgroup primitive_subgroup in
        let ignore_macro_primitives =
          if X.ignore_macro_primitives primitive_subgroup
          then " && PRIMITIVE_LEVEL != \"MACRO\""
          else ""
        in
        Out_channel.fprintf
          tcl_file
          "set v_%s_%s [llength [%s -filter {PRIMITIVE_GROUP == \"%s\" && \
           PRIMITIVE_SUBGROUP == \"%s\"%s}]]\n"
          primitive_group_name
          primitive_subgroup_name
          get_cells
          primitive_group_name
          primitive_subgroup_name
          ignore_macro_primitives;
        Out_channel.fprintf
          tcl_file
          "puts $report_file \"SUBGROUP %s:%s $v_%s_%s\"\n"
          primitive_group_name
          primitive_subgroup_name
          primitive_group_name
          primitive_subgroup_name)
    in
    match primitive_group with
    | Advanced g -> write_subgroups (module Primitive_group.Advanced) g
    | Arithmetic g -> write_subgroups (module Primitive_group.Arithmetic) g
    | Blockram g -> write_subgroups (module Primitive_group.Blockram) g
    | Clb g -> write_subgroups (module Primitive_group.Clb) g
    | Clock g -> write_subgroups (module Primitive_group.Clock) g
    | Configuration g -> write_subgroups (module Primitive_group.Configuration) g
    | Io g -> write_subgroups (module Primitive_group.Io) g
    | Register g -> write_subgroups (module Primitive_group.Register) g)
;;

let query_timing ~clocks ~tcl_file =
  List.iter clocks ~f:(fun clock ->
    let clock = Clock.name clock in
    Out_channel.fprintf
      tcl_file
      "set setup 0\n\
       set hold 0\n\
       set clock [get_clocks -filter {NAME == \"%s\"}]\n\
       if { [llength $clock] == 1 } {\n\
      \  set timing_path [get_timing_paths -from $clock -to $clock -max_paths 1 -nworst \
       1 -setup]\n\
      \  if { [llength $timing_path] == 1 } {\n\
      \    set setup [get_property SLACK $timing_path]\n\
      \  }\n\
      \  set timing_path [get_timing_paths -from $clock -to $clock -max_paths 1 -nworst \
       1 -hold]\n\
      \  if { [llength $timing_path] == 1 } {\n\
      \    set hold [get_property SLACK $timing_path]\n\
      \  }\n\
       }\n\
       puts $report_file \"TIMING %s $setup $hold\"\n"
      clock
      clock)
;;

let write_run_script
  ~(config : Config.t)
  ~top_name
  ~output_path
  ~part_name
  ~verilog_file
  ~xdc_file
  ~tcl_file
  ~clocks
  ~report_file
  ~place
  ~route
  ~checkpoint
  ~preserve_hierarchy
  =
  let report predicate extension =
    if predicate
    then
      Some (File_path_and_name.create ~path:output_path ~file_name:top_name ~extension ())
    else None
  in
  let utilization_report = report config.vivado_utilization_report ".utilization" in
  let timing_report = report config.vivado_timing_report ".timing" in
  let tcl_file = Out_channel.create (File_path_and_name.full_path_name tcl_file) in
  Out_channel.fprintf tcl_file "# Synthesis script generated by hardcaml_xilinx_reports\n";
  Out_channel.fprintf tcl_file "proc synthesize { root } {\n";
  Out_channel.fprintf tcl_file "create_project -in_memory -part %s\n" part_name;
  (* read files *)
  Out_channel.fprintf
    tcl_file
    "read_verilog %s\n"
    (File_path_and_name.tcl_rooted_file_name verilog_file);
  Out_channel.fprintf
    tcl_file
    "read_xdc %s\n"
    (File_path_and_name.tcl_rooted_file_name xdc_file);
  (* Make sure we detect xpm's if they are used. *)
  Out_channel.fprintf tcl_file "set_property top %s [current_fileset]\n" top_name;
  Out_channel.fprintf tcl_file "auto_detect_xpm\n";
  (* Match js_board's non-project synthesis flow. *)
  let maybe_retiming = if config.retiming then "-global_retiming on" else "" in
  let maybe_preserve_hierarchy =
    if preserve_hierarchy then "-flatten_hierarchy none" else ""
  in
  Out_channel.fprintf
    tcl_file
    "synth_design %s -top %s -mode out_of_context %s %s\n"
    (Option.value ~default:"" config.additional_synth_args)
    top_name
    maybe_retiming
    maybe_preserve_hierarchy;
  let maybe_write_checkpoint stage =
    if checkpoint
    then (
      let file = output_path ^ "/post_" ^ stage ^ ".dcp" in
      Out_channel.fprintf tcl_file "write_checkpoint -force \"%s\"\n" file)
  in
  maybe_write_checkpoint "synth";
  (match config.opt_design with
   | Some true -> Out_channel.fprintf tcl_file "opt_design\n"
   | Some false | None -> ());
  Out_channel.fprintf tcl_file "}\n";
  (* write reports *)
  Out_channel.fprintf tcl_file "proc write_reports { stage root } {\n";
  Option.iter utilization_report ~f:(fun utilization_report_file_name ->
    let utilization_report_file_name =
      File_path_and_name.prefix_file_name utilization_report_file_name "${stage}_"
    in
    Out_channel.fprintf
      tcl_file
      "report_utilization -file %s\n"
      (File_path_and_name.tcl_rooted_file_name utilization_report_file_name));
  Option.iter timing_report ~f:(fun timing_report_file_name ->
    let timing_report_file_name =
      File_path_and_name.prefix_file_name timing_report_file_name "${stage}_"
    in
    Out_channel.fprintf
      tcl_file
      "report_timing_summary -file %s\n"
      (File_path_and_name.tcl_rooted_file_name timing_report_file_name));
  (* query utilization *)
  let report_file = File_path_and_name.prefix_file_name report_file "${stage}_" in
  Out_channel.fprintf
    tcl_file
    "set report_file [open \"%s\" \"w\"]\n"
    (File_path_and_name.tcl_rooted_file_name report_file);
  Out_channel.fprintf tcl_file "current_instance\n";
  query_utilization ~config ~tcl_file ~primitive_groups:config.primitive_groups;
  (* query timing *)
  query_timing ~clocks ~tcl_file;
  Out_channel.fprintf tcl_file "close $report_file\n";
  Out_channel.fprintf tcl_file "}\n";
  Out_channel.fprintf tcl_file "synthesize \"%s\"\n" output_path;
  Out_channel.fprintf tcl_file "write_reports \"post_synth\" \"%s\"\n" output_path;
  if place
  then (
    Out_channel.fprintf tcl_file "place_design\n";
    Out_channel.fprintf tcl_file "write_reports \"post_place\" \"%s\"\n" output_path;
    maybe_write_checkpoint "place");
  if route
  then (
    Out_channel.fprintf tcl_file "route_design\n";
    Out_channel.fprintf tcl_file "write_reports \"post_route\" \"%s\"\n" output_path;
    maybe_write_checkpoint "route");
  Out_channel.close tcl_file
;;

let mkdir_if_needed path =
  match Unix.stat path with
  | exception Unix.Unix_error (ENOENT, _, _) ->
    (* Ok, create the directory *)
    Unix.mkdir path 0o755
  | { st_kind = S_DIR; _ } -> () (* directory already exists *)
  | _ -> raise_s [%message "Cannot create output directory" (path : string)]
;;

let create
  ?(place = false)
  ?(route = false)
  ?(checkpoint = false)
  ~io_delays
  ~clocks
  ~part_name
  ~output_path
  ~preserve_hierarchy
  ~verilog_file
  ~top_name
  config
  =
  mkdir_if_needed output_path;
  (* file names *)
  let xdc_file =
    File_path_and_name.create ~path:output_path ~file_name:top_name ~extension:".xdc" ()
  in
  let tcl_file =
    File_path_and_name.create ~path:output_path ~file_name:top_name ~extension:".tcl" ()
  in
  let report_file =
    File_path_and_name.create ~path:output_path ~file_name:"report" ~extension:".txt" ()
  in
  (* Write xdc constraints file *)
  write_xdc ~xdc_file ~clocks ~io_delays;
  (* Write the TCL script *)
  write_run_script
    ~config
    ~top_name
    ~output_path
    ~part_name
    ~verilog_file
    ~xdc_file
    ~tcl_file
    ~clocks
    ~report_file
    ~place
    ~route
    ~checkpoint
    ~preserve_hierarchy;
  (* Set report_file to the latest stage. *)
  let report_file =
    match place, route with
    | true, false -> File_path_and_name.prefix_file_name report_file "post_place_"
    | true, true -> File_path_and_name.prefix_file_name report_file "post_route_"
    | _, _ -> File_path_and_name.prefix_file_name report_file "post_synth_"
  in
  { tcl_file; report_file; output_path }
;;

module Hardcaml = struct
  module Config = struct
    type t =
      { common : Config.t
      ; full_design_hierarchy : bool
      }
    [@@deriving sexp_of]

    let default = { common = Config.default; full_design_hierarchy = false }
  end

  let create
    ?database
    ?(config = Config.default)
    ?place
    ?route
    ?checkpoint
    ?(io_delays = Map.empty (module String))
    ~clocks
    ~part_name
    ~output_path
    ~preserve_hierarchy
    circuit
    =
    let port_directions =
      let ports_with_direction direction ports =
        List.concat_map ports ~f:(fun port ->
          List.map (Signal.names port) ~f:(fun name -> name, direction))
      in
      Map.of_alist_exn
        (module String)
        (ports_with_direction `Input (Circuit.inputs circuit)
         @ ports_with_direction `Output (Circuit.outputs circuit)
         @ List.map (Circuit.phantom_inputs circuit) ~f:(fun (name, _) -> name, `Input))
    in
    let io_delays =
      Map.mapi io_delays ~f:(fun ~key:port ~data:delay ->
        match Map.find port_directions port with
        | Some direction -> direction, delay
        | None -> raise_s [%message "Unknown I/O delay port" (port : string)])
    in
    let opt_design =
      Option.value config.common.opt_design ~default:config.full_design_hierarchy
    in
    let config =
      { config with common = { config.common with opt_design = Some opt_design } }
    in
    mkdir_if_needed output_path;
    let top_name = Circuit.name circuit in
    let verilog_file =
      File_path_and_name.create ~path:output_path ~file_name:top_name ~extension:".v" ()
    in
    let rtl = Rtl.create ?database Verilog [ circuit ] in
    Out_channel.write_all
      (File_path_and_name.full_path_name verilog_file)
      ~data:
        (rtl
         |> (if config.full_design_hierarchy
             then Rtl.full_hierarchy
             else Rtl.top_levels_and_blackboxes)
         |> Rope.to_string);
    create
      ?place
      ?route
      ?checkpoint
      ~io_delays
      ~clocks
      ~part_name
      ~output_path
      ~preserve_hierarchy
      ~verilog_file
      ~top_name
      config.common
  ;;
end

module Verilog = struct
  let create
    ?(config = Config.default)
    ?place
    ?route
    ?checkpoint
    ~clocks
    ~part_name
    ~output_path
    ~preserve_hierarchy
    ~verilog_file:user_verilog_file
    ~top_name
    ()
    =
    (* The verilog file needs to be a top-level file in the output dir to work with the
       generate scripts. *)
    let verilog_file =
      File_path_and_name.create
        ~path:output_path
        ~file_name:(Core.Filename.basename user_verilog_file)
        ()
    in
    if not (String.equal (Core.Filename.dirname user_verilog_file) output_path)
    then
      Out_channel.write_all
        (File_path_and_name.full_path_name verilog_file)
        ~data:(In_channel.read_all user_verilog_file);
    create
      ?place
      ?route
      ?checkpoint
      ~io_delays:(Map.empty (module String))
      ~clocks
      ~part_name
      ~output_path
      ~preserve_hierarchy
      ~verilog_file
      ~top_name
      config
  ;;
end

let run ?(verbose = false) ?(path_to_vivado = "vivado") t =
  let command =
    Printf.sprintf
      "%s -mode batch -nolog -nojournal -source %s"
      path_to_vivado
      (File_path_and_name.full_path_name t.tcl_file)
  in
  let command = if verbose then command else command ^ " > /dev/null" in
  (* Stdio.printf "executing...'%s'\n%!" command; *)
  match%map.Async.Deferred Async.Unix.system command with
  | Ok () ->
    Some (Report.read ~file_name:(File_path_and_name.full_path_name t.report_file))
  | Error (_ : Core_unix.Exit_or_signal.error) -> None
;;

let output_path t = t.output_path
