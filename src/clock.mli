(** Define properties of clocks on the top level module. The most important properties are
    the clock net name and it's period.

    We also allow specification of the input BUFG driving the clock pin. This is optional,
    but allows the synthesizer to approximate skew across the device. It is specified at a
    bufg location constraint ie [BUFGCTRL_X0Y2]. *)

open! Import

type t

(** Create a clock with the period specified in nanoseconds. *)
val create : ?clk_src_bufg:string -> name:string -> period:float -> unit -> t

(** Create a clock with the frequency specified in MHz. *)
val create_mhz : ?clk_src_bufg:string -> name:string -> frequency_mhz:float -> unit -> t

val name : t -> string
val period : t -> float
val clk_src_bufg : t -> string option
