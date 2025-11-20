(** Primitive group definitions for Xilinx Vivado Ultrascale designs. See ug974.

    Groups and subgroups define properties of cells within the Vivado database. They are
    used to access properties of a cells and nets. In particular, here we are interested
    in properties related to utilization.

    They are used by selecting all cells in a module, then filtering those with the
    appropriate property. The number of such cells left is the corresponding total
    utilization count.

    The most important groups are Clb, Register and Blockram. *)

open! Import

module type Subgroup = sig
  type t

  val primitive_subgroup : t -> string
  val ignore_macro_primitives : t -> bool
end

(** {4 Primtive subgroups} *)
module Advanced : sig
  type t =
    | Mac
    | Gt
    | Interlaken
    | Pcie
    | Sysmon
  [@@deriving enumerate, sexp_of, variants]

  val primitive_subgroup : t -> string
  val ignore_macro_primitives : t -> bool
end

module Arithmetic : sig
  type t = Dsp [@@deriving enumerate, sexp_of, variants]

  val primitive_subgroup : t -> string
  val ignore_macro_primitives : t -> bool
end

module Blockram : sig
  type t =
    | Fifo
    | Bram
    | Uram
  [@@deriving enumerate, sexp_of, variants]

  val primitive_subgroup : t -> string
  val ignore_macro_primitives : t -> bool
end

module Clb : sig
  type t =
    | Latch
    | Carry
    | Lut
    | Muxf
    | Lutram
    | Srl
  [@@deriving enumerate, sexp_of, variants]

  val primitive_subgroup : t -> string
  val ignore_macro_primitives : t -> bool
end

module Clock : sig
  type t =
    | Buffer
    | Mux
    | Pll
  [@@deriving enumerate, sexp_of, variants]

  val primitive_subgroup : t -> string
  val ignore_macro_primitives : t -> bool
end

module Configuration : sig
  type t =
    | Bscan
    | Dna
    | Efuse
    | Ecc
    | Icap
    | Master_jtag
    | Startup
  [@@deriving enumerate, sexp_of, variants]

  val primitive_subgroup : t -> string
  val ignore_macro_primitives : t -> bool
end

module Io : sig
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

  val primitive_subgroup : t -> string
  val ignore_macro_primitives : t -> bool
end

module Register : sig
  type t =
    | Sdr
    | Metastability
    | Ddr
    | Latch
  [@@deriving enumerate, sexp_of, variants]

  val primitive_subgroup : t -> string
  val ignore_macro_primitives : t -> bool
end

(** {4 Primitive groups} *)
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

val primitive_group : t -> string
