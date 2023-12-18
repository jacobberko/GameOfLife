(* TEST PLAN:
   OUNIT vs MANUALLY TESTED:
   We attempted to test a majority of our system using automated
   OUnit tests. Much of our system simply depends on displaying
   things to the terminal interface, however, and we could not directly
   test those features using OUnit testing. For our OUnit tests, we
   attempted to test everything that works with the back-end of our
   game. For example, we used OUnit to test:
   - Creating user profiles.
   - Moving a user's position on the board.
   - Adding money to a user's profile.
   - Adding majors to a user's profile.
   - Testing edge cases on the board.
   And we manually tested:
   - Ensuring that the terminal displayed the proper scenario.
   - Making sure that the terminal interface lacked bugs.
   - Making sure that gameplay flowed smoothly for two players.
   TEST DEVELOPMENT:
   User Tests:
   Our User tests were mostly done via a black-box strategy. The reason
   that we felt comfortable doing this was because most of the aspects
   of the user profile are data types that do not display any odd
   behaviors at any particular corner cases. Much of the user's profile
   consisted of integers and strings, and we felt comfortable using
   blackbox tests for a majority of this module.
   That being said, certain portions of the user profile relied on
   glass-box tests in order to be effectively tested. In particular,
   we relied on glass box tests to test the major and position attributes
   of a user. The major attribute was a list, a data type that is
   hidden from the client, and thus we relied on glassbox testing.
   The position attribute was modified by an operation on another
   attribute of the user's profile, something that is not apparent
   to the client, so we used glassbox testing to ensure that our test
   was right.
   Board Tests:
   The tests for the Board module were strictly manual, as the board
   is just a data file being read in.
   BoardGame Tests:
   The tests for BoardGame were done in a strictly blackbox fashion.
   The reason that we developed our tests in this manner was because
   every modifying action in the BoardGame module was a call for a
   function in the User module. Feeling confident that our User module
   was adequately tested, we used inductive reasoning to believe that
   our BoardGame module worked properly.
   Majors Tests:
   The tests for Majors were done in a glass-box fashion as the
   representation type for majors is hidden from the client.
   DiceRolls Tests:
   The DiceRolls module was tested in a glass-box fashion for the same
   reasons as we employed that strategy for the Majors module.

   WHY WE BELIEVE OUR SYSTEM IS CORRECT:
   We strongly believe that our tests display the correctness of our system
   via a semi-inductive logical process. Our entire system builds off of
   each other, with the majority of our functionality being dependent upon
   the User module. We rigorously tested the user module and every corner
   case that we can think of, and based on the fact that our game repeats
   in equal increments around the corner cases, we are sure that our system works
   for an infinite number of iterations of the game. *)

open OUnit2
module Test_User = Game.User.User1

(** Building user types.*)
let test_user_1 = Test_User.build "Jack"

let test_user_2 = Test_User.build "Mary"
let test_user_3 = Test_User.build "Alyssa"

let tests =
  "Tests for User"
  >::: [
         (* Build Tests *)
         ( "Testing build on a simple user." >:: fun _ ->
           assert_equal "Jack" (Test_User.get_name test_user_1) );
         ( "Testing build's assignment of money on a new user" >:: fun _ ->
           assert_equal 0 (Test_User.get_money test_user_2) );
         ( "Testing initial position of a new user" >:: fun _ ->
           assert_equal 1 (Test_User.get_position test_user_1) );
         ( "Testing build on another simple user." >:: fun _ ->
           assert_equal "Mary" (Test_User.get_name test_user_2) );
         ( "Testing increment_money on a simple user." >:: fun _ ->
           assert_equal 20
             (Test_User.get_money (Test_User.increment_money test_user_1 20)) );
         ( "Testing increment_money on a user with non-zero money." >:: fun _ ->
           assert_equal 40
             (Test_User.get_money
                (Test_User.increment_money
                   (Test_User.increment_money test_user_1 20)
                   20)) );
         (* Testing Board Position *)
         ( "Testing simple increment of position" >:: fun _ ->
           assert_equal 12
             (Test_User.get_position
                (11 |> Test_User.update_position test_user_1)) );
         ( "Testing simple increment of position (different user)" >:: fun _ ->
           assert_equal 12
             (Test_User.get_position
                (11 |> Test_User.update_position test_user_3)) );
         ( "Testing a compound set of rolls on a user" >:: fun _ ->
           assert_equal 13
             (Test_User.get_position
                (Test_User.update_position
                   (Test_User.update_position test_user_1 4)
                   8)) );
         ( "Testing a compound set of rolls on different user" >:: fun _ ->
           assert_equal 11
             (Test_User.get_position
                (Test_User.update_position
                   (Test_User.update_position test_user_1 4)
                   6)) );
         (* Testing Major Selection *)
         ( "Testing removing a major" >:: fun _ ->
           assert_equal 3
             (List.length
                (Game.Majors.Major.remove_major
                   [ "Math"; "Computer Science"; "English"; "History" ]
                   "Computer Science")) );
         ( "Testing removing a major from the beginning of the list" >:: fun _ ->
           assert_equal "Computer Science English History "
             (Game.Majors.Major.print_list
                (Game.Majors.Major.remove_major
                   [ "Math"; "Computer Science"; "English"; "History" ]
                   "Math")) );
         ( "Testing removing a major from the beggining of the list (length)"
         >:: fun _ ->
           assert_equal 3
             (List.length
                (Game.Majors.Major.remove_major
                   [ "Math"; "Computer Science"; "English"; "History" ]
                   "Math")) );
         ( "Testing removing a major from the middle of the list" >:: fun _ ->
           assert_equal "Math English History "
             (Game.Majors.Major.print_list
                (Game.Majors.Major.remove_major
                   [ "Math"; "Computer Science"; "English"; "History" ]
                   "Computer Science")) );
         ( "Testing removing a major from the middle of the list (length)"
         >:: fun _ ->
           assert_equal 3
             (List.length
                (Game.Majors.Major.remove_major
                   [ "Math"; "Computer Science"; "English"; "History" ]
                   "English")) );
         ( "Testing removing a major from the end of the list" >:: fun _ ->
           assert_equal "Math Computer Science English "
             (Game.Majors.Major.print_list
                (Game.Majors.Major.remove_major
                   [ "Math"; "Computer Science"; "English"; "History" ]
                   "History")) );
         ( "Testing removing a major from the end of the list (length)"
         >:: fun _ ->
           assert_equal 3
             (List.length
                (Game.Majors.Major.remove_major
                   [ "Math"; "Computer Science"; "English"; "History" ]
                   "History")) );
         ( "Testing removing a major when there is only one major" >:: fun _ ->
           assert_equal ""
             (Game.Majors.Major.print_list
                (Game.Majors.Major.remove_major [ "Math" ] "Math")) );
         ( "Testing removing a major when there is only one (length)"
         >:: fun _ ->
           assert_equal 0
             (List.length
                (Game.Majors.Major.remove_major [ "English" ] "English")) );
         ( "Testing removing a major with printing" >:: fun _ ->
           assert_equal "Math English History "
             (Game.Majors.Major.print_list
                (Game.Majors.Major.remove_major
                   [ "Math"; "Computer Science"; "English"; "History" ]
                   "Computer Science")) );
         ( "Testing first element corner case on remove " >:: fun _ ->
           assert_equal 2
             (List.length
                (Game.Majors.Major.remove_major [ "CS"; "Math"; "Art" ] "CS"))
         );
         ( "Testing corner case with printing" >:: fun _ ->
           assert_equal "Math Art "
             (Game.Majors.Major.print_list
                (Game.Majors.Major.remove_major [ "CS"; "Math"; "Art" ] "CS"))
         );
         ( "Testing longer list corner case where removal occurs at end of list "
         >:: fun _ ->
           assert_equal 5
             (List.length
                (Game.Majors.Major.remove_major
                   [
                     "CS";
                     "Math";
                     "History";
                     "Food Science";
                     "Economics";
                     "English";
                   ]
                   "English")) );
         ( "Testing longer list end of list corner case with printing"
         >:: fun _ ->
           assert_equal "CS Math History Food Science Economics "
             (Game.Majors.Major.print_list
                (Game.Majors.Major.remove_major
                   [
                     "CS";
                     "Math";
                     "History";
                     "Food Science";
                     "Economics";
                     "English";
                   ]
                   "English")) );
         (*Testing sum of rolls*)
         ( "Testing initial sum of rolls for a new user" >:: fun _ ->
           assert_equal 0 (Test_User.get_sum_of_rolls test_user_1) );
         ( "Testing initial sum of rolls for a different user" >:: fun _ ->
           assert_equal 0 (Test_User.get_sum_of_rolls test_user_3) );
         ( "Testing sum of rolls after one update" >:: fun _ ->
           let updated_user = Test_User.update_position test_user_1 5 in
           assert_equal 5 (Test_User.get_sum_of_rolls updated_user) );
         ( "Testing sum of rolls after multiple updates" >:: fun _ ->
           let updated_user = Test_User.update_position test_user_1 3 in
           let updated_user_final = Test_User.update_position updated_user 4 in
           assert_equal 7 (Test_User.get_sum_of_rolls updated_user_final) );
         ( "Testing sum of rolls with exact board length roll" >:: fun _ ->
           let updated_user = Test_User.update_position test_user_1 30 in
           assert_equal 30 (Test_User.get_sum_of_rolls updated_user) );
         ( "Testing sum of rolls over multiple turns" >:: fun _ ->
           let updated_user = Test_User.update_position test_user_1 4 in
           let updated_user_final = Test_User.update_position updated_user 6 in
           assert_equal 10 (Test_User.get_sum_of_rolls updated_user_final) );
         ( "Testing sum of rolls after crossing start position" >:: fun _ ->
           let updated_user = Test_User.update_position test_user_1 35 in
           assert_equal 35 (Test_User.get_sum_of_rolls updated_user) );
         ( "Testing sum of rolls with 0 roll" >:: fun _ ->
           let updated_user = Test_User.update_position test_user_1 0 in
           assert_equal 0 (Test_User.get_sum_of_rolls updated_user) );
         ( "Testing sum of rolls with 0 roll (differnt user)" >:: fun _ ->
           let updated_user = Test_User.update_position test_user_2 0 in
           assert_equal 0 (Test_User.get_sum_of_rolls updated_user) );
         ( "Testing sum of rolls after two rolls" >:: fun _ ->
           let updated_user = Test_User.update_position test_user_1 5 in
           let updated_user = Test_User.update_position updated_user 4 in
           assert_equal 9 (Test_User.get_sum_of_rolls updated_user) );
         ( "Testing sum of rolls after two rolls (different user)" >:: fun _ ->
           let updated_user = Test_User.update_position test_user_2 5 in
           let updated_user = Test_User.update_position updated_user 4 in
           assert_equal 9 (Test_User.get_sum_of_rolls updated_user) );
         ( "Testing sum of rolls after multiple rolls" >:: fun _ ->
           let updated_user = Test_User.update_position test_user_1 5 in
           let updated_user = Test_User.update_position updated_user 4 in
           let updated_user_final = Test_User.update_position updated_user 3 in
           assert_equal 12 (Test_User.get_sum_of_rolls updated_user_final) );
         ( "Testing sum of rolls after multiple large rolls" >:: fun _ ->
           let updated_user = Test_User.update_position test_user_1 15 in
           let updated_user = Test_User.update_position updated_user 20 in
           let updated_user_final = Test_User.update_position updated_user 25 in
           assert_equal 60 (Test_User.get_sum_of_rolls updated_user_final) );
         ( "Testing sum of rolls with multiple small rolls" >:: fun _ ->
           let updated_user =
             List.fold_left Test_User.update_position test_user_1 [ 2; 3; 4; 1 ]
           in
           assert_equal 10 (Test_User.get_sum_of_rolls updated_user) );
         ( "Testing sum of rolls with a large single roll" >:: fun _ ->
           let updated_user = Test_User.update_position test_user_1 50 in
           assert_equal 50 (Test_User.get_sum_of_rolls updated_user) );
         (*Testing ptofile information*)
         ( "Testing profile basic" >:: fun _ ->
           let profile = Test_User.profile test_user_1 in
           assert_equal ("Jack", 0, 1) profile );
         ( "Testing profile basic (different user)" >:: fun _ ->
           let profile = Test_User.profile test_user_2 in
           assert_equal ("Mary", 0, 1) profile );
         ( "Testing profile information after changing name" >:: fun _ ->
           let updated_user = Test_User.update_profile "Jill" 0 1 0 [] [] in
           let profile = Test_User.profile updated_user in
           assert_equal ("Jill", 0, 1) profile );
         ( "Testing profile information after changing name (different user)"
         >:: fun _ ->
           let updated_user = Test_User.update_profile "Stacy" 0 1 0 [] [] in
           let profile = Test_User.profile updated_user in
           assert_equal ("Stacy", 0, 1) profile );
         ( "Testing profile with non-zero money" >:: fun _ ->
           let updated_user = Test_User.update_profile "Jack" 100 1 0 [] [] in
           let profile = Test_User.profile updated_user in
           assert_equal ("Jack", 100, 1) profile );
         ( "Testing profile with non-one psition" >:: fun _ ->
           let updated_user = Test_User.update_profile "Jack" 100 3 0 [] [] in
           let profile = Test_User.profile updated_user in
           assert_equal ("Jack", 100, 3) profile );
         ( "Testing profile information with changed position" >:: fun _ ->
           let updated_user = Test_User.update_profile "Jack" 0 5 0 [] [] in
           let profile = Test_User.profile updated_user in
           assert_equal ("Jack", 0, 5) profile );
         ( "Testing profile information with changed position and name"
         >:: fun _ ->
           let updated_user = Test_User.update_profile "Karen" 0 5 0 [] [] in
           let profile = Test_User.profile updated_user in
           assert_equal ("Karen", 0, 5) profile );
         ( "Testing profile information with changed name and money" >:: fun _ ->
           let updated_user = Test_User.update_profile "Karen" 100 1 0 [] [] in
           let profile = Test_User.profile updated_user in
           assert_equal ("Karen", 100, 1) profile );
         ( "Testing profile information with changed position and money"
         >:: fun _ ->
           let updated_user = Test_User.update_profile "Jack" 100 1 0 [] [] in
           let profile = Test_User.profile updated_user in
           assert_equal ("Jack", 100, 1) profile );
         ( "Testing profile information with changed position, money and name"
         >:: fun _ ->
           let updated_user = Test_User.update_profile "Karen" 100 5 0 [] [] in
           let profile = Test_User.profile updated_user in
           assert_equal ("Karen", 100, 5) profile );
         ( "Testing profile information with updated sum" >:: fun _ ->
           let updated_user = Test_User.update_profile "Jack" 0 1 20 [] [] in
           let profile = Test_User.profile updated_user in
           assert_equal ("Jack", 0, 1) profile );
       ]

let _ = run_test_tt_main tests
