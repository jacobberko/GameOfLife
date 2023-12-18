(* Survey for player information *)
let read_input prompt =
  print_string prompt;
  read_line ()

let ask_year () = read_input "What year are you in [1, 2, 3, 4]? "

let ask_gpa () =
  read_input "What is your GPA? (Only if you are not a freshman) [1, 2, 3, 4] "

let ask_major () =
  read_input "What is your school? [Arts, Engineering, Agriculture]"

let ask_favorite_spot_on_campus () =
  read_input "What's your favorite spot on campus?"

let ask_dream_job () = read_input "What is your dream job after graduation?"

let ask_hobby_or_club () =
  read_input "What is your favorite hobby or club at Cornell?"

let ask_study_abroad () =
  let answer =
    read_input "Are you planning to or have you studied abroad? (Yes/No) "
  in
  if answer = "Yes" then
    Some (read_input "Where did you go or where would you like to go? ")
  else None

let survey () =
  let year = ask_year () in
  let gpa = if year <> "Freshman" then ask_gpa () else "N/A" in
  let major = ask_major () in
  let favorite_spot = ask_favorite_spot_on_campus () in
  let dream_job = ask_dream_job () in
  let hobby_or_club = ask_hobby_or_club () in
  let study_abroad_location = ask_study_abroad () in
  let study_abroad_message =
    match study_abroad_location with
    | Some location -> "Study Abroad Location: " ^ location
    | None -> "No Study Abroad Plans"
  in
  Printf.printf
    "\n\
     Survey Results:\n\
     Year: %s\n\
     GPA: %s\n\
     Major: %s\n\
     Favorite Spot on Campus: %s\n\
     Dream Job: %s\n\
     Favorite Hobby or Club: %s\n\
     %s\n"
    year gpa major favorite_spot dream_job hobby_or_club study_abroad_message

let rec print_list lst =
  match lst with [] -> "" | h :: t -> h ^ " " ^ print_list t

(** Main Game Loop*)
let rec main_loop_one_player dice_rolls user =
  print_endline "";
  match dice_rolls with
  | 0 ->
      print_endline "Final Results: ";
      print_endline ("User: " ^ Game.User.User1.get_name user);
      print_endline ("Money: " ^ string_of_int (Game.User.User1.get_money user))
  | x -> (
      print_endline
        "Enter Go To Keep Going or V to view your profile or B to view the \
         board. Any other command will quit the game:";
      let next_entry = read_line () in
      match next_entry with
      | "Go" ->
          print_endline "Rolling Again...";
          let roll1, roll2 =
            Game.Dicerolls.Dice.roll Game.Dicerolls.Dice.build
          in

          let bonus_points =
            if roll1 = 1 && roll2 = 1 then (
              print_endline
                ("You rolled: " ^ string_of_int roll1 ^ " and "
               ^ string_of_int roll2 ^ ". This is double 1's! 10 extra points");
              10)
            else if roll1 = 2 && roll2 = 2 then (
              print_endline
                ("You rolled: " ^ string_of_int roll1 ^ " and "
               ^ string_of_int roll2 ^ ". This is double 2's! 10 extra points");
              10)
            else if roll1 = 3 && roll2 = 3 then (
              print_endline
                ("You rolled: " ^ string_of_int roll1 ^ " and "
               ^ string_of_int roll2 ^ ". This is double 3's! 10 extra points");
              10)
            else if roll1 = 4 && roll2 = 4 then (
              print_endline
                ("You rolled: " ^ string_of_int roll1 ^ " and "
               ^ string_of_int roll2 ^ ". This is double 4's! 10 extra points");
              10)
            else if roll1 = 5 && roll2 = 5 then (
              print_endline
                ("You rolled: " ^ string_of_int roll1 ^ " and "
               ^ string_of_int roll2 ^ ". This is double 5's! 10 extra points");
              10)
            else if roll1 = 6 && roll2 = 6 then (
              print_endline
                ("You rolled: " ^ string_of_int roll1 ^ " and "
               ^ string_of_int roll2 ^ ". This is double 6's! 10 extra points");
              10)
            else (
              print_endline
                ("You rolled: " ^ string_of_int roll1 ^ " and "
               ^ string_of_int roll2 ^ ". ");
              0)
          in

          let updated_user =
            Game.User.User1.update_position user (roll1 + roll2)
          in

          (* Call handle_event to update the user based on their new position *)
          let new_majors, new_minors, new_money =
            Game.Boardgame.handle_event updated_user
          in

          let final_money = new_money + bonus_points in
          (* Update the user object with new values *)
          let new_user =
            Game.User.User1.update_profile
              (Game.User.User1.get_name updated_user)
              final_money
              (Game.User.User1.get_position updated_user)
              (Game.User.User1.get_sum_of_rolls updated_user)
              new_majors new_minors
          in
          (* Continue the game loop with the updated user *)
          main_loop_one_player (x - 1) new_user
      | "V" ->
          print_endline ("User: " ^ Game.User.User1.get_name user);
          print_endline "";
          print_endline
            ("Money: " ^ string_of_int (Game.User.User1.get_money user));
          print_endline "";
          print_endline
            ("Position: " ^ string_of_int (Game.User.User1.get_position user));
          print_endline
            ("Majors: " ^ print_list (Game.User.User1.view_majors user));
          print_endline "___________________________________________";
          main_loop_one_player x user
      | "B" ->
          print_endline
            (Game.Board.GameBoard.print "finalproject/data/gameboard.txt");
          print_endline "___________________________________________";
          main_loop_one_player x user
      | _ ->
          print_endline "Unrecognized character. Ending Game...";
          main_loop_one_player 0 user)

let rec main_loop_two_player dice_rolls user1 user2 =
  print_endline "";
  match dice_rolls with
  | 0 ->
      print_endline "Final Results:";
      print_endline ("User: " ^ Game.User.User1.get_name user1);
      print_endline ("Money: " ^ string_of_int (Game.User.User1.get_money user1));
      print_endline ("User: " ^ Game.User.User1.get_name user2);
      print_endline ("Money: " ^ string_of_int (Game.User.User1.get_money user2))
  | x -> (
      print_endline
        "Enter Go To Keep Going or V to view your profile or B to view the \
         board. Any other command will quit the game:";
      let next_entry = read_line () in
      match next_entry with
      | "Go" ->
          let roll1_player1, roll2_player1 =
            Game.Dicerolls.Dice.roll Game.Dicerolls.Dice.build
          in
          print_endline "Player 1 Rolling...";
          let bonus_points_1 =
            if roll1_player1 = 1 && roll2_player1 = 1 then (
              print_endline
                ("You rolled: "
                ^ string_of_int roll1_player1
                ^ " and "
                ^ string_of_int roll2_player1
                ^ ". This is double 1's! 10 extra points");
              10)
            else if roll1_player1 = 2 && roll2_player1 = 2 then (
              print_endline
                ("You rolled: "
                ^ string_of_int roll1_player1
                ^ " and "
                ^ string_of_int roll2_player1
                ^ ". This is double 2's! 10 extra points");
              10)
            else if roll1_player1 = 3 && roll2_player1 = 3 then (
              print_endline
                ("You rolled: "
                ^ string_of_int roll1_player1
                ^ " and "
                ^ string_of_int roll2_player1
                ^ ". This is double 3's! 10 extra points");
              10)
            else if roll1_player1 = 4 && roll2_player1 = 4 then (
              print_endline
                ("You rolled: "
                ^ string_of_int roll1_player1
                ^ " and "
                ^ string_of_int roll2_player1
                ^ ". This is double 4's! 10 extra points");
              10)
            else if roll1_player1 = 5 && roll2_player1 = 5 then (
              print_endline
                ("You rolled: "
                ^ string_of_int roll1_player1
                ^ " and "
                ^ string_of_int roll2_player1
                ^ ". This is double 5's! 10 extra points");
              10)
            else if roll1_player1 = 6 && roll2_player1 = 6 then (
              print_endline
                ("You rolled: "
                ^ string_of_int roll1_player1
                ^ " and "
                ^ string_of_int roll2_player1
                ^ ". This is double 6's! 10 extra points");
              10)
            else (
              print_endline
                ("You rolled: "
                ^ string_of_int roll1_player1
                ^ " and "
                ^ string_of_int roll2_player1
                ^ ". ");
              0)
          in
          let updated_user_1 =
            Game.User.User1.update_position user1 (roll1_player1 + roll2_player1)
          in

          let new_majors1, new_minors1, new_money1 =
            Game.Boardgame.handle_event updated_user_1
          in
          let final_money1 = new_money1 + bonus_points_1 in
          let new_user_1 =
            Game.User.User1.update_profile
              (Game.User.User1.get_name updated_user_1)
              final_money1
              (Game.User.User1.get_position updated_user_1)
              (Game.User.User1.get_sum_of_rolls updated_user_1)
              new_majors1 new_minors1
          in

          (*PLAYER TWO*)
          let roll1_player2, roll2_player2 =
            Game.Dicerolls.Dice.roll Game.Dicerolls.Dice.build
          in
          print_endline "Player 2 Rolling...";
          let bonus_points_2 =
            if roll1_player2 = 1 && roll2_player2 = 1 then (
              print_endline
                ("You rolled: "
                ^ string_of_int roll1_player2
                ^ " and "
                ^ string_of_int roll2_player2
                ^ ". This is double 1's! 10 extra points");
              10)
            else if roll1_player2 = 2 && roll2_player2 = 2 then (
              print_endline
                ("You rolled: "
                ^ string_of_int roll1_player2
                ^ " and "
                ^ string_of_int roll2_player2
                ^ ". This is double 2's! 10 extra points");
              10)
            else if roll1_player2 = 3 && roll2_player2 = 3 then (
              print_endline
                ("You rolled: "
                ^ string_of_int roll1_player2
                ^ " and "
                ^ string_of_int roll2_player2
                ^ ". This is double 3's! 10 extra points");
              10)
            else if roll1_player2 = 4 && roll2_player2 = 4 then (
              print_endline
                ("You rolled: "
                ^ string_of_int roll1_player2
                ^ " and "
                ^ string_of_int roll2_player2
                ^ ". This is double 4's! 10 extra points");
              10)
            else if roll1_player2 = 5 && roll2_player2 = 5 then (
              print_endline
                ("You rolled: "
                ^ string_of_int roll1_player2
                ^ " and "
                ^ string_of_int roll2_player2
                ^ ". This is double 5's! 10 extra points");
              10)
            else (
              print_endline
                ("You rolled: "
                ^ string_of_int roll1_player2
                ^ " and "
                ^ string_of_int roll2_player2
                ^ ". ");
              0)
          in

          let updated_user_2 =
            Game.User.User1.update_position user2 (roll1_player2 + roll2_player2)
          in

          (* Call handle_event to update the user based on their new position *)
          let new_majors2, new_minors2, new_money2 =
            Game.Boardgame.handle_event updated_user_2
          in

          let final_money2 = new_money2 + bonus_points_2 in

          (* Update the user object with new values *)
          let new_user_2 =
            Game.User.User1.update_profile
              (Game.User.User1.get_name updated_user_2)
              final_money2
              (Game.User.User1.get_position updated_user_2)
              (Game.User.User1.get_sum_of_rolls updated_user_2)
              new_majors2 new_minors2
          in
          (* Continue the game loop with the updated user *)
          main_loop_two_player (x - 1) new_user_1 new_user_2
      | "V" ->
          print_endline ("Player 1: " ^ Game.User.User1.get_name user1);
          print_endline
            ("Money: " ^ string_of_int (Game.User.User1.get_money user1));
          print_endline
            ("Position: " ^ string_of_int (Game.User.User1.get_position user1));
          print_endline ("Player 2: " ^ Game.User.User1.get_name user2);
          print_endline
            ("Money: " ^ string_of_int (Game.User.User1.get_money user2));
          print_endline
            ("Position: " ^ string_of_int (Game.User.User1.get_position user2));
          main_loop_two_player x user1 user2
      | "B" ->
          print_endline
            (Game.Board.GameBoard.print "finalproject/data/gameboard.txt");
          main_loop_two_player x user1 user2
      | _ ->
          print_endline "Unrecognized character. Ending Game...";
          main_loop_two_player 0 user1 user2)

(* Main Game Terminal Interface*)
let () =
  print_endline "Welcome to the Game of Life!";
  print_endline "___________________________________________________";
  print_endline "How many players are playing (1 or 2)?";
  let num_players = read_line () in
  print_endline "___________________________________________________";
  print_endline "Please enter start to begin the game.";
  let game_begin = read_line () in
  print_endline "";
  print_endline "___________________________________________________";
  match (game_begin, num_players) with
  | "start", "1" ->
      print_endline "Game Beginning!";
      print_endline "";
      print_endline "Please enter your name:";
      let username = read_line () in
      print_endline ("Your name is " ^ username);
      print_endline "_________________________________________________";
      print_endline
        "Before we start, let's complete a quick survey for Player 1!";
      survey ();
      main_loop_one_player 20 (Game.User.User1.build username)
  | "start", "2" ->
      print_endline "Game Beginning!";
      print_endline "Please enter player 1's name:";
      let username_1 = read_line () in
      print_endline "Please enter player 2's name:";
      let username_2 = read_line () in
      print_endline
        "Before we start, let's complete a quick survey for Player 1!";
      survey ();
      print_endline "Now, let's complete the survey for Player 2!";
      survey ();
      print_endline
        ("Ok, " ^ username_1 ^ " & " ^ username_2 ^ "! Let's start playing!.");
      main_loop_two_player 20
        (Game.User.User1.build username_1)
        (Game.User.User1.build username_2)
  | _ -> print_endline "Unrecognized command. Please restart."
