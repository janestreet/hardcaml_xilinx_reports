open! Import

module Config = struct
  type t =
    { vivado_utilization_report : bool
    ; vivado_timing_report : bool
    ; primitive_groups : Primitive_group.t list
    ; full_design_hierarchy : bool
    ; opt_design : bool option
    ; report_hierarchy : bool
    ; retiming : bool
    }
  [@@deriving sexp_of]

  let default =
    { vivado_utilization_report = false
    ; vivado_timing_report = false
    ; primitive_groups = [ Clb []; Register [] ]
    ; full_design_hierarchy = false
    ; opt_design = None
    ; report_hierarchy = true
    ; retiming = true
    }
  ;;
end

type t =
  { tcl_file : File_path_and_name.t
  ; report_file : File_path_and_name.t
  ; output_path : string
  }

let write_xdc ~xdc_file ~clocks =
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
    then Some (File_path_and_name.create ~path:output_path ~file_name:top_name ~extension)
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
  (* run synthesis with retiming *)
  if config.retiming
  then
    Out_channel.fprintf
      tcl_file
      "set_param synth.elaboration.rodinMoreOptions \"rt::set_parameter synRetiming true\"\n";
  let maybe_preserve_hierarchy =
    if preserve_hierarchy then "-flatten_hierarchy none" else ""
  in
  Out_channel.fprintf
    tcl_file
    "synth_design -top %s -mode out_of_context %s\n"
    top_name
    maybe_preserve_hierarchy;
  let maybe_write_checkpoint stage =
    if checkpoint
    then (
      let file = output_path ^ "/post_" ^ stage ^ ".dcp" in
      Out_channel.fprintf tcl_file "write_checkpoint -force \"%s\"\n" file)
  in
  maybe_write_checkpoint "synth";
  (match config.opt_design, config.full_design_hierarchy with
   | Some true, (true | false) | None, true -> Out_channel.fprintf tcl_file "opt_design\n"
   | Some false, (true | false) | None, false -> ());
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
  ?database
  ?(config = Config.default)
  ?(place = false)
  ?(route = false)
  ?(checkpoint = false)
  ~clocks
  ~part_name
  ~output_path
  ~preserve_hierarchy
  circuit
  =
  mkdir_if_needed output_path;
  let top_name = Circuit.name circuit in
  (* file names *)
  let verilog_file =
    File_path_and_name.create ~path:output_path ~file_name:top_name ~extension:".v"
  in
  let xdc_file =
    File_path_and_name.create ~path:output_path ~file_name:top_name ~extension:".xdc"
  in
  let tcl_file =
    File_path_and_name.create ~path:output_path ~file_name:top_name ~extension:".tcl"
  in
  let report_file =
    File_path_and_name.create ~path:output_path ~file_name:"report" ~extension:".txt"
  in
  (* Write xdc constraints file *)
  write_xdc ~xdc_file ~clocks;
  (* Write verilog RTL *)
  let rtl = Rtl.create ?database Verilog [ circuit ] in
  Out_channel.write_all
    (File_path_and_name.full_path_name verilog_file)
    ~data:
      (rtl
       |> (if config.full_design_hierarchy
           then Rtl.full_hierarchy
           else Rtl.top_levels_and_blackboxes)
       |> Rope.to_string);
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
