(** Board represents the game board. *)
module type Board = sig
  type t
  (** The representation type for the game board. *)

  val print : string -> string
  (** A function that can print the game board. *)
end

module GameBoard : Board
