open! Import

type t =
  { name : string
  ; period : float
  ; clk_src_bufg : string option
  }
[@@deriving sexp_of, fields ~getters]

let create ?clk_src_bufg ~name ~period () = { name; period; clk_src_bufg }

let create_mhz ?clk_src_bufg ~name ~frequency_mhz () =
  create ?clk_src_bufg ~name ~period:(1000. /. frequency_mhz) ()
;;
