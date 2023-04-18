open! Core
open Hardcaml
open Signal

module type Sequential_interface = sig
  module Data : Hardcaml.Interface.S
  include Hardcaml.Interface.S

  val clock : 'a t -> 'a
  val clear : 'a t -> 'a
  val data : 'a t -> 'a Data.t
  val create : clock:'a -> clear:'a -> data:'a Data.t -> 'a t
end

module Make_sequential (I : Sequential_interface) (O : Interface.S) = struct
  let create create (scope : Scope.t) (i : _ I.t) =
    let spec = Reg_spec.create ~clock:(I.clock i) ~clear:(I.clear i) () in
    let inputs =
      I.create
        ~clock:(I.clock i)
        ~clear:(I.clear i)
        ~data:(I.Data.map (I.data i) ~f:(reg spec ~enable:vdd))
    in
    let outputs = create scope inputs in
    O.map outputs ~f:(reg spec ~enable:vdd)
  ;;

  let hier ~name create_fn scope (i : _ I.t) =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name (create create_fn) i
  ;;

  let create_list create_fns scope (i : _ I.t) =
    List.fold
      ~init:(O.Of_signal.of_int 0)
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
    [@@deriving sexp_of, hardcaml]
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
      ~init:(O.Of_signal.of_int 0)
      ~f:(fun o1 o2 -> O.map2 o1 o2 ~f:( |: ))
      (List.map create_fns ~f:(fun (name, create_fn) -> hier ~name create_fn scope i))
  ;;
end
