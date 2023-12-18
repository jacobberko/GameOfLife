module type Majors = sig
  type t

  val build : t
  val pick_major : string
  val remove_major : string list -> string -> string list
  val print_list : string list -> string
end

module Major = struct
  type t = string list

  let build = [ "Chemistry"; "Computer Science"; "Math"; "English" ]

  let pick_major =
    Random.init 1000;
    List.nth build (Random.int 4)

  let remove_major lst x = List.filter (fun c -> c <> x) lst

  let rec print_list lst =
    match lst with [] -> "" | h :: t -> h ^ " " ^ print_list t
end
