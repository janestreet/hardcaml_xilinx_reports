open! Import

module type Subgroup = sig
  type t

  val primitive_subgroup : t -> string
  val ignore_macro_primitives : t -> bool
end

(* Primtive subgroups *)
module Advanced = struct
  type t =
    | Mac
    | Gt
    | Interlaken
    | Pcie
    | Sysmon
  [@@deriving enumerate, sexp_of, variants]

  let primitive_subgroup t = Variants.to_name t |> String.uppercase
  let ignore_macro_primitives _ = false
end

module Arithmetic = struct
  type t = Dsp [@@deriving enumerate, sexp_of, variants]

  let primitive_subgroup t = Variants.to_name t |> String.uppercase
  let ignore_macro_primitives _ = false
end

module Blockram = struct
  type t =
    | Fifo
    | Bram
    | Uram
  [@@deriving enumerate, sexp_of, variants]

  let primitive_subgroup t = Variants.to_name t |> String.uppercase
  let ignore_macro_primitives _ = false
end

module Clb = struct
  type t =
    | Latch
    | Carry
    | Lut
    | Muxf
    | Lutram
    | Srl
  [@@deriving enumerate, sexp_of, variants]

  let primitive_subgroup t = Variants.to_name t |> String.uppercase

  (* Vivado includes MACRO primitives for LUTRAMs which are then expanded into INTERNAL
     primitives. To avoid double-counting we exclude MACRO primitives. *)
  let ignore_macro_primitives = function
    | Lutram -> true
    | _ -> false
  ;;
end

module Clock = struct
  type t =
    | Buffer
    | Mux
    | Pll
  [@@deriving enumerate, sexp_of, variants]

  let primitive_subgroup t = Variants.to_name t |> String.uppercase
  let ignore_macro_primitives _ = false
end

module Configuration = struct
  type t =
    | Bscan
    | Dna
    | Efuse
    | Ecc
    | Icap
    | Master_jtag
    | Startup
  [@@deriving enumerate, sexp_of, variants]

  let primitive_subgroup t = Variants.to_name t |> String.uppercase
  let ignore_macro_primitives _ = false
end

module Io = struct
  type t =
    | Bitslice
    | Dci_reset
    | Input_buffer
    | Delay
    | Bidir_buffer
    | Serdes
    | Weak_driver
    | Output_buffer
    | Sdr
    | Metastability
    | Ddr
    | Latch
  [@@deriving enumerate, sexp_of, variants]

  let primitive_subgroup t = Variants.to_name t |> String.uppercase
  let ignore_macro_primitives _ = false
end

module Register = struct
  type t =
    | Sdr
    | Metastability
    | Ddr
    | Latch
  [@@deriving enumerate, sexp_of, variants]

  let primitive_subgroup t = Variants.to_name t |> String.uppercase
  let ignore_macro_primitives _ = false
end

(* Primitive groups *)
type t =
  | Advanced of Advanced.t list
  | Arithmetic of Arithmetic.t list
  | Blockram of Blockram.t list
  | Clb of Clb.t list
  | Clock of Clock.t list
  | Configuration of Configuration.t list
  | Io of Io.t list
  | Register of Register.t list
[@@deriving sexp_of, variants]

let primitive_group = function
  | Advanced _ -> "ADVANCED"
  | Arithmetic _ -> "ARITHMETIC"
  | Blockram _ -> "BLOCKRAM"
  | Clb _ -> "CLB"
  | Clock _ -> "CLOCK"
  | Configuration _ -> "CONFIGURATION"
  | Io _ -> "IO"
  | Register _ -> "REGISTER"
;;
