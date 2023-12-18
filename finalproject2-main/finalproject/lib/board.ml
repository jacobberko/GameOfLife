module type Board = sig
  type t

  val print : string -> string
end

module GameBoard : Board = struct
  type t = int array

  (* A function that takes elements from a list and produces a string. *)
  let rec concat (lst : string list) =
    match lst with [] -> "" | h :: t -> h ^ "\n" ^ concat t

  let print file =
    let contents = In_channel.with_open_bin file In_channel.input_all in
    let lst = String.split_on_char '\n' contents in
    concat lst
end
