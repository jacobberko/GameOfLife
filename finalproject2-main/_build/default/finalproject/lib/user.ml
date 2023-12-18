module type User = sig
  type t
  (** Representation type. *)

  val build : string -> t
  (** Given a username [str] builds a user of type t.*)

  val get_name : t -> string

  (** Given a user of type t, returns the user's name.*)

  val get_money : t -> int
  val get_position : t -> int
  val get_sum_of_rolls : t -> int
  val increment_money : t -> int -> t
  val profile : t -> string * int * int

  val update_profile :
    string -> int -> int -> int -> string list -> string list -> t

  val update_position : t -> int -> t
  val view_majors : t -> string list
end

module User1 = struct
  type t = {
    name : string;
    money : int;
    position : int;
    sum_of_rolls : int;
    majors : string list;
    minors : string list;
  }

  (** Given a username [str] builds a user of type t.*)
  let build input =
    {
      name = input;
      money = 0;
      position = 1;
      sum_of_rolls = 0;
      majors = [];
      minors = [];
    }

  let get_name t = t.name
  let get_money t = t.money
  let get_position t = t.position
  let get_sum_of_rolls t = t.sum_of_rolls

  let increment_money user incr =
    {
      name = user.name;
      money = user.money + incr;
      position = user.position;
      sum_of_rolls = user.sum_of_rolls;
      majors = user.majors;
      minors = user.minors;
    }

  let profile t = (t.name, t.money, t.position)

  let update_profile in_name in_money in_pos in_sum in_majors in_minors =
    {
      name = in_name;
      money = in_money;
      position = in_pos;
      sum_of_rolls = in_sum;
      majors = in_majors;
      minors = in_minors;
    }

  let update_position t result =
    {
      name = t.name;
      money = t.money;
      position =
        (match (t.position + result) mod 30 with
        | 0 -> 30
        | _ -> (t.position + result) mod 30);
      sum_of_rolls = t.sum_of_rolls + result;
      majors =
        (let new_pos =
           match (t.sum_of_rolls + result) mod 30 with
           | 0 -> 30
           | _ -> (t.sum_of_rolls + result) mod 30
         in
         match new_pos mod 10 with
         | 1 -> Majors.Major.pick_major :: t.majors
         | _ -> t.majors);
      minors = t.minors;
    }

  let view_majors t = t.majors
end
