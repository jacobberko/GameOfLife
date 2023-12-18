val shuffle : 'a list -> 'a list
(** The shuffle function takes a list and returns a new list with the elements shuffled. *)

val initial_position : int
(** Initial position for the game. *)

val initial_money : int
(** Initial amount of money for the game. *)

val majors : (string * string * int) list
(** A list of majors, each associated with a college and a money bonus. *)

val chosen_majors : string list
(** A list of chosen majors, initially empty. *)

val chosen_minors : string list
(** A list of chosen minors, initially empty. *)

val minors : (string * string * int) list
(** A list of minors, each associated with a college and a money bonus. *)

val take : int -> 'a list -> 'a list
(** The take function takes an integer n and a list l, and returns the first n elements of l. *)

val pick_major : unit -> string list * int
(** The pick_major function allows the player to pick a major from a shuffled list of majors. *)

val pick_minor : unit -> string list * int
(** The pick_minor function allows the player to pick a minor from a shuffled list of minors. *)

val handle_event : User.User1.t -> string list * string list * int
(** The handle_event function takes a game position, lists of chosen majors and minors, the current money, dorms, the pick_minor function, and the minors list,
   and returns an updated game state based on the event at the given position. *)
