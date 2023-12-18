(** DiceRolls is a simulator for dice. *)
module type Dicerolls = sig
  type t
  (** Representation type for a set of dice.*)

  val build : t
  val roll : t -> int * int
end

module Dice : Dicerolls
