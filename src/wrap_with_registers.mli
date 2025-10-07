(** Instantiate one more more circuits, wrapping the input and output ports with
    registers.

    This can then be used to generate a synthesis report with accurate timing numbers
    (including input and output delays) using the [blackbox=none] and [hier=true]
    synthesis flow. *)
open Hardcaml

(** Returns a circuit that instantiates the circuits, where the input and output ports are
    registered, with the exception of the clock port. *)
val wrap_circuit_with_registers
  :  ?instantiation_attributes:Rtl_attribute.t list
  -> clock_port_name:string
  -> scope:Scope.t
  -> Circuit.t
  -> Circuit.t

module Make_sequential
    (I : sig
       include Hardcaml.Interface.S

       val get_clock : 'a t -> 'a
       val set_clock : 'a t -> clock:'a -> 'a t
     end)
    (O : Interface.S) : sig
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
    [@@deriving hardcaml]
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
