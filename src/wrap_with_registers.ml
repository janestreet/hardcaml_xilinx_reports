open! Core
open Hardcaml
open Signal

let assert_singleton = function
  | [ x ] -> x
  | _ -> failwith "assert_singleton"
;;

let wrap_circuit_with_registers
  ?instantiation_attributes
  ~clock_port_name
  ~scope
  (circuit : Circuit.t)
  =
  let subcircuit_name =
    let circuit_database = Scope.circuit_database scope in
    Circuit_database.insert circuit_database circuit
  in
  let clock = Signal.input "clock" 1 in
  let spec = Reg_spec.create ~clock () in
  let (instantiation : Instantiation.t) =
    Instantiation.create
      ?attributes:instantiation_attributes
      ~inputs:
        (List.map (Circuit.inputs circuit) ~f:(fun s ->
           let name = assert_singleton (Signal.names s) in
           let w = Signal.width s in
           let input_wire =
             if String.equal name clock_port_name
             then clock
             else Signal.reg spec (Signal.input name w)
           in
           name, input_wire))
      ~outputs:
        (List.map (Circuit.outputs circuit) ~f:(fun s ->
           let name = assert_singleton (Signal.names s) in
           let width = Signal.width s in
           name, width))
      ~name:subcircuit_name
      ()
  in
  let outputs =
    List.map
      (Map.to_alist (Instantiation.outputs instantiation))
      ~f:(fun (name, signal) -> Signal.output name (Signal.reg spec signal))
  in
  Circuit.create_exn ~name:(Circuit.name circuit ^ "_wrap_with_registers") outputs
;;

module Make_sequential
    (I : sig
       include Hardcaml.Interface.S

       val get_clock : 'a t -> 'a
       val set_clock : 'a t -> clock:'a -> 'a t
     end)
    (O : Interface.S) =
struct
  let create create (scope : Scope.t) (i : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let clock = I.get_clock i in
    let spec = Reg_spec.create ~clock () in
    let inputs =
      I.set_clock
        ~clock
        (I.map2 I.port_names i ~f:(fun name s -> reg spec s -- (name ^ "_reg")))
    in
    let outputs = create scope inputs in
    O.map2 O.port_names outputs ~f:(fun name s -> reg spec s -- (name ^ "_reg"))
  ;;

  let hier ~name create_fn scope (i : _ I.t) =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name (create create_fn) i
  ;;

  let create_list create_fns scope (i : _ I.t) =
    List.fold
      ~init:(O.Of_signal.zero ())
      ~f:(fun o1 o2 -> O.map2 o1 o2 ~f:( |: ))
      (List.map create_fns ~f:(fun (name, create_fn) -> hier ~name create_fn scope i))
  ;;
end

module Make (I : Interface.S) (O : Interface.S) = struct
  module I_with_clock = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; i : 'a I.t [@rtlprefix "i_"]
      }
    [@@deriving hardcaml ~rtlmangle:false]
  end

  type combinational_or_sequential =
    [ `Combinational of Scope.t -> Interface.Create_fn(I)(O).t
    | `Sequential of Scope.t -> Interface.Create_fn(I_with_clock)(O).t
    ]

  let create create scope (i : _ I_with_clock.t) =
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let inputs = I.map i.i ~f:(reg spec ~enable:vdd) in
    let outputs =
      match create with
      | `Combinational create -> create scope inputs
      | `Sequential create -> create scope { i with i = inputs }
    in
    O.map outputs ~f:(reg spec ~enable:vdd)
  ;;

  let hier ~name create_fn scope (i : _ I_with_clock.t) =
    let module Scoped = Hierarchy.In_scope (I_with_clock) (O) in
    Scoped.hierarchical ~scope ~name (create create_fn) i
  ;;

  let create_list create_fns scope (i : _ I_with_clock.t) =
    List.fold
      ~init:(O.Of_signal.zero ())
      ~f:(fun o1 o2 -> O.map2 o1 o2 ~f:( |: ))
      (List.map create_fns ~f:(fun (name, create_fn) -> hier ~name create_fn scope i))
  ;;
end
