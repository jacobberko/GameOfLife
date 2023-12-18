module type Dicerolls = sig
  type t
  (** Representation type for a set of dice.*)

  val build : t
  val roll : t -> int * int
end

module Dice : Dicerolls = struct
  type t = int list

  (* Modified to represent a six-sided dice *)
  let build : t = [ 1; 2; 3; 4; 5; 6 ]

  (* Rolls a die using OCaml's random number generator. *)
  let roll die =
    Random.self_init ();
    let roll1 = List.nth die (Random.int 6) in
    let roll2 = List.nth die (Random.int 6) in
    (roll1, roll2)
end
