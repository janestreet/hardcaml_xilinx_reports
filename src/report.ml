open! Import

module Subgroup = struct
  type t =
    { name : string
    ; value : int
    }
  [@@deriving sexp_of]
end

module Group = struct
  type t =
    { name : string
    ; value : int
    ; subgroups : Subgroup.t list
    }
  [@@deriving sexp_of]
end

module Clock = struct
  type t =
    { name : string
    ; setup : float
    ; hold : float
    }
  [@@deriving sexp_of]
end

type t =
  { groups : Group.t list
  ; clocks : Clock.t list
  }
[@@deriving sexp_of]

let parse_line line =
  match String.split ~on:' ' line |> List.filter ~f:(Fn.non String.is_empty) with
  | [ "GROUP"; name; value ] ->
    `group { Group.name; value = Int.of_string value; subgroups = [] }
  | [ "SUBGROUP"; name; value ] ->
    `subgroup { Subgroup.name; value = Int.of_string value }
  | [ "TIMING"; name; setup; hold ] ->
    `timing { Clock.name; setup = Float.of_string setup; hold = Float.of_string hold }
  | bad_line -> raise_s [%message "Could not parse report file." (bad_line : string list)]
;;

let group_name_of_subgroup name =
  match String.split ~on:':' name with
  | [ group; _subgroup ] -> group
  | _ -> raise_s [%message "Invalid subgroup name" (name : string)]
;;

let subgroup_name_of_subgroup name =
  match String.split ~on:':' name with
  | [ _group; subgroup ] -> subgroup
  | _ -> raise_s [%message "Invalid subgroup name" (name : string)]
;;

let read ~file_name =
  let lines = In_channel.read_lines file_name in
  let lines = List.map lines ~f:parse_line in
  let groups =
    List.filter_map lines ~f:(function
      | `group g -> Some g
      | _ -> None)
  in
  let subgroups =
    List.filter_map lines ~f:(function
      | `subgroup s -> Some s
      | _ -> None)
  in
  let clocks =
    List.filter_map lines ~f:(function
      | `timing f -> Some f
      | _ -> None)
  in
  (* Finally, associate subgroups under groups *)
  let map = Map.of_alist_exn (module String) (List.map groups ~f:(fun g -> g.name, g)) in
  let map =
    List.fold subgroups ~init:map ~f:(fun map sub ->
      let group = group_name_of_subgroup sub.name in
      match Map.find map group with
      | Some group' ->
        Map.set map ~key:group ~data:{ group' with subgroups = sub :: group'.subgroups }
      | None -> map
      (* we could fail here also, as this should not happen *))
  in
  let groups = Map.data map in
  { groups; clocks }
;;

let list_hierarchically ~top_level_name ~circuits ~f =
  let circuits =
    List.map circuits ~f:(fun circuit -> Circuit.name circuit, circuit)
    |> Map.of_alist_exn (module String)
  in
  let ret = ref [] in
  let rec loop ~level circuit_name =
    match Map.find circuits circuit_name with
    | None -> ()
    | Some circuit ->
      List.iter (Circuit.instantiations circuit) ~f:(fun instantiation ->
        let level = level + 1 in
        let this =
          f
            ~level
            ~instance_label:(Some instantiation.instance_label)
            ~module_name:instantiation.circuit_name
        in
        ret := this :: !ret;
        loop ~level instantiation.circuit_name)
  in
  ret := [ f ~level:0 ~instance_label:None ~module_name:top_level_name ];
  loop ~level:0 top_level_name;
  List.rev !ret
;;

let mk_name ~level ~module_name ~instance_label =
  let indent = String.init level ~f:(Fn.const ' ') in
  match instance_label with
  | None -> [%string "%{indent}%{module_name}"]
  | Some instance_label -> [%string "%{indent}%{module_name} (inst = %{instance_label})"]
;;

let print_utilization_table
  ~file
  ~top_level_name
  ~circuits
  (reports : (string * t option) list)
  =
  let header = List.find reports ~f:(fun (_, report) -> Option.is_some report) in
  let header_row =
    match header with
    | None -> []
    | Some (_, report) ->
      let report = Option.value_exn report in
      let groups = report.groups in
      let header =
        List.map groups ~f:(fun group ->
          group.name
          :: List.map group.subgroups ~f:(fun subgroup ->
            subgroup_name_of_subgroup subgroup.name))
      in
      (* Leading dash will make the column left-align. *)
      "-NAME" :: List.concat header
  in
  match header_row with
  | [] | [ _ ] -> ()
  | _ ->
    let num_fields = List.length header_row - 1 in
    let reports = Map.of_alist_exn (module String) reports in
    let row_of_report ~level ~instance_label ~module_name =
      let name = mk_name ~level ~module_name ~instance_label in
      match (Map.find reports module_name : t option option) with
      | None | Some None -> name :: List.init num_fields ~f:(Fn.const "-")
      | Some (Some report) ->
        let row =
          List.map report.groups ~f:(fun group ->
            group.value :: List.map group.subgroups ~f:(fun subgroup -> subgroup.value))
          |> List.map ~f:(List.map ~f:Int.to_string)
        in
        name :: List.concat row
    in
    let rows = list_hierarchically ~top_level_name ~circuits ~f:row_of_report in
    Ascii_table.simple_list_table ~oc:file header_row rows
;;

let print_timing_table
  ~file
  ~top_level_name
  ~circuits
  (reports : (string * t option) list)
  =
  (* All clocks found in the design. *)
  let all_clocks =
    List.fold
      reports
      ~init:(Set.empty (module String))
      ~f:(fun set (_, t) ->
        match t with
        | None -> set
        | Some t ->
          List.fold t.clocks ~init:set ~f:(fun set clock -> Set.add set clock.name))
    |> Set.to_list
  in
  let num_clocks = List.length all_clocks in
  let reports = Map.of_alist_exn (module String) reports in
  let row_of_report ~level ~instance_label ~module_name =
    let find_clock aclock clocks =
      match
        List.find clocks ~f:(fun (clock : Clock.t) -> String.equal clock.name aclock)
      with
      | None -> "-"
      | Some clock -> Float.to_string clock.setup ^ "/" ^ Float.to_string clock.hold
    in
    let name = mk_name ~level ~module_name ~instance_label in
    match Map.find reports module_name with
    | None | Some None -> name :: List.init num_clocks ~f:(Fn.const "-")
    | Some (Some report) ->
      let clocks = report.clocks in
      name :: List.map all_clocks ~f:(fun aclock -> find_clock aclock clocks)
  in
  let header_rows =
    (* Leading dash will make the column left-align. *)
    "-NAME" :: all_clocks
  in
  let rows = list_hierarchically ~top_level_name ~circuits ~f:row_of_report in
  Ascii_table.simple_list_table ~oc:file header_rows rows
;;
