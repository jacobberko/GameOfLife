(** A module type to represent the different majors the game supports. *)
module type Majors = sig
  type t
  (** The representation type for the majors. *)

  val build : t
  (** Builds the major set of type t. *)

  val pick_major : string
  (** Picks a major from the set of majors. *)

  val remove_major : string list -> string -> string list
  (** A function to remove a major from the set of majors. *)

  val print_list : string list -> string
  (** A function to print an OCaml list. *)
end

module Major : Majors
