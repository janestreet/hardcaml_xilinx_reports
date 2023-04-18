(** Utility module for managing files which are referenced from the current directory and
    vivado project directory. *)
open! Import

type t

val create : path:string -> file_name:string -> extension:string -> t
val full_path_name : t -> string
val tcl_rooted_file_name : t -> string
val prefix_file_name : t -> string -> t
