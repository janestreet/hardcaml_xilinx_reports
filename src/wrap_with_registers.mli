(** Instantiate one more more circuits, wrapping the input and output ports with
    registers.

    This can then be used to generate a synthesis report with accurate timing numbers
    (including input and output delays) using the [blackbox=none] and [hier=true]
    synthesis flow. *)
open Hardcaml

module type Sequential_interface = sig
  module Data : Hardcaml.Interface.S
  include Hardcaml.Interface.S

  val clock : 'a t -> 'a
  val clear : 'a t -> 'a
  val data : 'a t -> 'a Data.t
  val create : clock:'a -> clear:'a -> data:'a Data.t -> 'a t
end

module Make_sequential (I : Sequential_interface) (O : Interface.S) : sig
  val create
    :  (Scope.t -> Interface.Create_fn(I)(O).t)
    -> Scope.t
    -> Interface.Create_fn(I)(O).t

  val hier
    :  name:string
    -> (Scope.t -> Interface.Create_fn(I)(O).t)
    -> Scope.t
    -> Interface.Create_fn(I)(O).t

  val create_list
    :  (string * (Scope.t -> Interface.Create_fn(I)(O).t)) list
    -> Scope.t
    -> Interface.Create_fn(I)(O).t
end

module Make (I : Interface.S) (O : Interface.S) : sig
  module I_with_clock : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; i : 'a I.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  type combinational_or_sequential =
    [ `Combinational of Scope.t -> Interface.Create_fn(I)(O).t
    | `Sequential of Scope.t -> Interface.Create_fn(I_with_clock)(O).t
    ]

  val create
    :  combinational_or_sequential
    -> Scope.t
    -> Interface.Create_fn(I_with_clock)(O).t

  val hier
    :  name:string
    -> combinational_or_sequential
    -> Scope.t
    -> Interface.Create_fn(I_with_clock)(O).t

  val create_list
    :  (string * combinational_or_sequential) list
    -> Scope.t
    -> Interface.Create_fn(I_with_clock)(O).t
end
