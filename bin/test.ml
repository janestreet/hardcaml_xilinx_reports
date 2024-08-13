(* open! Core *)
module Synth = Hardcaml_xilinx_reports

module Design = struct
  open Hardcaml
  open Signal

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; a : 'a [@bits 8]
      ; b : 'a [@bits 8]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { o : 'a [@bits 8] } [@@deriving hardcaml]
  end

  let _create (i : _ I.t) =
    let spec = Reg_spec.create () ~clock:i.clock ~clear:i.clear in
    { O.o = reg spec ~enable:vdd (reg spec ~enable:vdd (i.a +: i.b)) }
  ;;
end

module Hierarchical_design = struct
  open Hardcaml
  open Signal

  module Mult = struct
    module I = Design.I
    module O = Design.O

    let create _scope (i : _ I.t) =
      let spec = Reg_spec.create () ~clock:i.clock ~clear:i.clear in
      { O.o = reg spec ~enable:vdd (uresize (i.a *: i.b) ~width:8) }
    ;;

    let hier scope (i : _ I.t) =
      let module Scoped = Hierarchy.In_scope (I) (O) in
      Scoped.hierarchical ~scope ~name:"multiplier8x8" create i
    ;;
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; a_real : 'a [@bits 8]
      ; a_imag : 'a [@bits 8]
      ; b_real : 'a [@bits 8]
      ; b_imag : 'a [@bits 8]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { c_real : 'a [@bits 8]
      ; c_imag : 'a [@bits 8]
      }
    [@@deriving hardcaml]
  end

  let create scope (i : _ I.t) =
    let spec = Reg_spec.create () ~clock:i.clock ~clear:i.clear in
    let rr =
      Mult.hier
        scope
        { clock = i.clock; clear = i.clear; Mult.I.a = i.a_real; b = i.b_real }
    in
    let ii =
      Mult.hier
        scope
        { clock = i.clock; clear = i.clear; Mult.I.a = i.a_imag; b = i.b_imag }
    in
    let ri =
      Mult.hier
        scope
        { clock = i.clock; clear = i.clear; Mult.I.a = i.a_real; b = i.b_imag }
    in
    let ir =
      Mult.hier
        scope
        { clock = i.clock; clear = i.clear; Mult.I.a = i.a_imag; b = i.b_real }
    in
    { O.c_real = reg spec ~enable:vdd (rr.o -: ii.o)
    ; c_imag = reg spec ~enable:vdd (ri.o +: ir.o)
    }
  ;;
end

module Command =
  Synth.Command.With_interface (Hierarchical_design.I) (Hierarchical_design.O)

let () = Command.command_run ~name:"mulme" Hierarchical_design.create
