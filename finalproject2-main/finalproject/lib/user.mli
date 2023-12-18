(** The basic framework for any singular player's profile. *)
module type User = sig
  type t
  (** Representation type. *)

  val build : string -> t
  (** Given a username [str] builds a user of type t.*)

  val get_name : t -> string
  (** Given a user of type t, returns the user's name.*)

  val get_money : t -> int
  (** Returns the amount of money belonging to a user. *)

  val get_position : t -> int
  (** Returns the position of a user. *)

  val get_sum_of_rolls : t -> int
  (** Returns the sum of the user's roll. *)

  val increment_money : t -> int -> t
  (** Increments the amount of money belonging to a user. *)

  val profile : t -> string * int * int
  (** Returns a profile of the user with their name, position, and money. *)

  val update_profile :
    string -> int -> int -> int -> string list -> string list -> t
  (** Updates the user's profile should their be a change in their status. *)

  val update_position : t -> int -> t
  (** Updates the position of the user based on a new roll. *)

  val view_majors : t -> string list
  (** Function to view the majors associated with a user. *)
end

module User1 : User
