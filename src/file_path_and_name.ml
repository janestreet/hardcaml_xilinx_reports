open! Import

type t =
  { file_name : string
  ; path : string
  }
[@@deriving sexp_of]

let ( /^ ) = Stdlib.Filename.concat

let create ?extension ~path ~file_name () =
  let file_name =
    match extension with
    | Some extension -> file_name ^ extension
    | None -> file_name
  in
  { path; file_name }
;;

let full_path_name t = t.path /^ t.file_name
let tcl_rooted_file_name t = "[file join $root " ^ t.file_name ^ "]"
let prefix_file_name t s = { t with file_name = s ^ t.file_name }
