let shuffle lst =
  let array = Array.of_list lst in
  let swap i j =
    let temp = array.(i) in
    array.(i) <- array.(j);
    array.(j) <- temp
  in
  for i = Array.length array - 1 downto 1 do
    swap i (Random.int (i + 1))
  done;
  Array.to_list array

let initial_position = 1
let initial_money = 10

let majors =
  [
    ("Africana Studies", "College of Arts and Sciences", 70);
    ("Agricultural Sciences", "College of Agriculture and Life Sciences", 80);
    ("American Studies", "College of Arts and Sciences", 70);
    ("Animal Science", "College of Agriculture and Life Sciences", 90);
    ("Anthropology", "College of Arts and Sciences", 70);
    ( "Applied Economics and Management",
      "College of Agriculture and Life Sciences, SC Johnson College of Business",
      130 );
    ("Archaeology", "College of Arts and Sciences", 70);
    ("Architecture", "College of Architecture, Art, and Planning", 110);
    ("Asian Studies", "College of Arts and Sciences", 70);
    ("Astronomy", "College of Arts and Sciences", 100);
    ("Atmospheric Science", "College of Agriculture and Life Sciences", 90);
    ( "Biological Engineering",
      "College of Agriculture and Life Sciences, College of Engineering",
      120 );
    ( "Biological Sciences",
      "College of Agriculture and Life Sciences, College of Arts and Sciences",
      100 );
    ( "Biology & Society",
      "College of Agriculture and Life Sciences, College of Arts and Sciences",
      90 );
    ("Biomedical Engineering", "College of Engineering", 140);
    ("Biometry and Statistics", "College of Agriculture and Life Sciences", 120);
    ("Chemical Engineering", "College of Engineering", 150);
    ("Chemistry", "College of Arts and Sciences", 100);
    ("China and Asia-Pacific Studies", "College of Arts and Sciences", 80);
    ("Civil Engineering", "College of Engineering", 130);
    ( "Classics (Classics, Classical Civ., Greek, Latin)",
      "College of Arts and Sciences",
      70 );
    ("Cognitive Science", "College of Arts and Sciences", 90);
    ("College Scholar", "College of Arts and Sciences", 80);
    ("Communication", "College of Agriculture and Life Sciences", 90);
    ("Comparative Literature", "College of Arts and Sciences", 70);
    ( "Computer Science",
      "College of Arts and Sciences, College of Engineering",
      150 );
    ("Design and Environmental Analysis", "College of Human Ecology", 100);
    ( "Earth and Atmospheric Sciences",
      "College of Agriculture and Life Sciences, College of Arts and Sciences, \
       College of Engineering",
      100 );
    ("Economics", "College of Arts and Sciences", 110);
    ("Electrical and Computer Engineering", "College of Engineering", 150);
    ("Engineering Physics", "College of Engineering", 140);
    ("English", "College of Arts and Sciences", 70);
    ("Entomology", "College of Agriculture and Life Sciences", 80);
    ( "Environment & Sustainability",
      "College of Agriculture and Life Sciences, College of Arts and Sciences",
      100 );
    ( "Environmental Engineering",
      "College of Agriculture and Life Sciences, College of Engineering",
      120 );
    ("Feminist, Gender & Sexuality Studies", "College of Arts and Sciences", 70);
    ("Fiber Science and Apparel Design", "College of Human Ecology", 90);
    ("Fine Arts", "College of Architecture, Art, and Planning", 80);
    ("Food Science", "College of Agriculture and Life Sciences", 100);
    ("French", "College of Arts and Sciences", 70);
    ("German Studies", "College of Arts and Sciences", 70);
    ( "Global & Public Health Sciences",
      "College of Agriculture and Life Sciences, College of Human Ecology",
      100 );
    ("Global Development", "College of Agriculture and Life Sciences", 80);
    ("Government", "College of Arts and Sciences", 80);
    ("Health Care Policy", "Cornell Jeb E. Brooks School of Public Policy", 110);
    ("History", "College of Arts and Sciences", 70);
    ( "History of Architecture (transfer students only)",
      "College of Architecture, Art, and Planning",
      100 );
    ("History of Art", "College of Arts and Sciences", 70);
    ( "Hotel Administration",
      "Cornell Peter and Stephanie Nolan School of Hotel Administration",
      130 );
    ("Human Biology, Health and Society", "College of Human Ecology", 100);
    ("Human Development", "College of Human Ecology", 90);
    ("Independent Major—Arts and Sciences", "College of Arts and Sciences", 80);
    ("Independent Major—Engineering", "College of Engineering", 120);
    ( "Industrial and Labor Relations",
      "School of Industrial and Labor Relations",
      110 );
    ( "Information Science",
      "College of Agriculture and Life Sciences, College of Arts and Sciences",
      120 );
    ( "Information Science, Systems, and Technology",
      "College of Engineering",
      140 );
    ("Interdisciplinary Studies", "College of Agriculture and Life Sciences", 80);
    ("Italian", "College of Arts and Sciences", 70);
    ("Jewish Studies", "College of Arts and Sciences", 70);
    ("Landscape Architecture", "College of Agriculture and Life Sciences", 90);
    ("Linguistics", "College of Arts and Sciences", 80);
    ("Materials Science and Engineering", "College of Engineering", 130);
    ("Mathematics", "College of Arts and Sciences", 110);
    ("Mechanical Engineering", "College of Engineering", 130);
    ("Music", "College of Arts and Sciences", 80);
    ("Near Eastern Studies", "College of Arts and Sciences", 70);
    ( "Nutritional Sciences",
      "College of Agriculture and Life Sciences, College of Human Ecology",
      100 );
    ("Operations Research and Engineering", "College of Engineering", 130);
    ("Performing and Media Arts", "College of Arts and Sciences", 80);
    ("Philosophy", "College of Arts and Sciences", 70);
    ("Physics", "College of Arts and Sciences", 110);
    ("Plant Sciences", "College of Agriculture and Life Sciences", 90);
    ( "Policy Analysis and Management",
      "Cornell Jeb E. Brooks School of Public Policy",
      110 );
    ("Psychology", "College of Arts and Sciences", 80);
    ("Religious Studies", "College of Arts and Sciences", 70);
    ("Science and Technology Studies", "College of Arts and Sciences", 80);
    ("Sociology", "College of Arts and Sciences", 80);
    ("Spanish", "College of Arts and Sciences", 70);
    ("Statistical Science", "College of Arts and Sciences", 110);
    ( "Urban and Regional Studies",
      "College of Architecture, Art, and Planning",
      90 );
    ("Viticulture and Enology", "College of Agriculture and Life Sciences", 90);
  ]

let chosen_majors = []
let chosen_minors = []

let minors =
  [
    ("Actuarial Science", "College of Arts and Sciences", 60);
    ("Aerospace Engineering", "College of Engineering", 70);
    ("Africana Studies", "College of Arts and Sciences", 20);
    ( "American Indian and Indigenous Studies",
      "College of Agriculture and Life Sciences",
      30 );
    ("American Sign Language", "College of Arts and Sciences", 10);
    ("American Studies", "College of Arts and Sciences", 20);
    ("Animal Science", "College of Agriculture and Life Sciences", 40);
    ("Anthropology", "College of Arts and Sciences", 20);
    ("Applied Economics", "SC Johnson College of Business", 60);
    ( "Applied Exercise Science",
      "College of Agriculture and Life Sciences, College of Human Ecology",
      40 );
    ("Applied Mathematics", "College of Engineering", 60);
    ("Arabic", "College of Arts and Sciences", 20);
    ("Archaeology", "College of Arts and Sciences", 20);
    ("Architecture", "College of Architecture, Art, and Planning", 50);
    ( "Artificial Intelligence",
      "Cornell Ann S. Bowers College of Computing and Information Science",
      70 );
    ("Asian American Studies", "College of Arts and Sciences", 20);
    ("Astrobiology", "College of Arts and Sciences", 40);
    ("Astronomy", "College of Arts and Sciences", 40);
    ("Atmospheric Sciences", "College of Engineering", 50);
    ( "Biological Engineering",
      "College of Agriculture and Life Sciences, College of Engineering",
      60 );
    ("Biomedical Engineering", "College of Engineering", 70);
    ("Biomedical Sciences", "College of Veterinary Medicine", 50);
    ("Biometry and Statistics", "College of Agriculture and Life Sciences", 50);
    ("Business", "SC Johnson College of Business", 60);
    ("China and Asia-Pacific Studies", "College of Arts and Sciences", 30);
    ("Civil Infrastructure", "College of Engineering", 50);
    ("Classical Civilization", "College of Arts and Sciences", 20);
    ("Classics", "College of Arts and Sciences", 20);
    ( "Climate Change",
      "College of Agriculture and Life Sciences, College of Engineering",
      50 );
    ("Cognitive Science", "College of Arts and Sciences", 40);
    ("Communication", "College of Agriculture and Life Sciences", 40);
    ("Community Food Systems", "College of Agriculture and Life Sciences", 30);
    ("Comparative Literature", "College of Arts and Sciences", 20);
    ( "Computer Science",
      "College of Arts and Sciences, College of Engineering",
      70 );
    ("Creative Writing", "College of Arts and Sciences", 30);
    ( "Crime, Prisons, Education, and Justice",
      "College of Arts and Sciences",
      30 );
    ("Crop Management", "College of Agriculture and Life Sciences", 40);
    ("Dance", "College of Arts and Sciences", 20);
    ( "Data Science",
      "Cornell Ann S. Bowers College of Computing and Information Science",
      70 );
    ("Data Science in Astronomy", "College of Arts and Sciences", 40);
    ("Demography", "Cornell Jeb E. Brooks School of Public Policy", 40);
    ("Design & Environmental Analysis", "College of Human Ecology", 40);
    ("Digital Agriculture", "College of Agriculture and Life Sciences", 50);
    ("Dyson Business Minor for Engineers", "SC Johnson College of Business", 60);
    ( "Dyson Business Minor for Life Sciences",
      "SC Johnson College of Business",
      60 );
    ( "Earth and Atmospheric Sciences",
      "College of Agriculture and Life Sciences, College of Arts and Sciences, \
       College of Engineering",
      50 );
    ("East Asian Studies", "College of Arts and Sciences", 30);
    ("Education", "College of Agriculture and Life Sciences", 30);
    ("Electrical and Computer Engineering", "College of Engineering", 70);
    ("Engineering Communications", "College of Engineering", 40);
    ("Engineering Management", "College of Engineering", 60);
    ("Entrepreneurship", "SC Johnson College of Business", 60);
    ("Entomology", "College of Agriculture and Life Sciences", 40);
    ( "Environmental Engineering",
      "College of Agriculture and Life Sciences, College of Engineering",
      60 );
    ( "Environment & Sustainability",
      "College of Agriculture and Life Sciences, College of Arts and Sciences",
      50 );
    ("European Studies", "Mario Einaudi Center for International Studies", 30);
    ("Fashion Studies", "College of Human Ecology", 40);
    ("Feminist, Gender, & Sexuality Studies", "College of Arts and Sciences", 20);
    ("Fiber Science", "College of Human Ecology", 40);
    ("Film", "College of Arts and Sciences", 30);
    ("Fine Arts", "College of Architecture, Art, and Planning", 40);
    ("Food and Agricultural Business", "SC Johnson College of Business", 60);
    ("Food Science", "College of Agriculture and Life Sciences", 50);
    ("French", "College of Arts and Sciences", 20);
    ("Fungal Biology", "College of Agriculture and Life Sciences", 40);
    ("Game Design", "College of Engineering", 60);
    ("German Studies", "College of Arts and Sciences", 20);
    ("Gerontology", "College of Human Ecology", 30);
    ("Global Asia Studies", "College of Arts and Sciences", 30);
    ("Global Health", "College of Human Ecology", 50);
    ("Health Policy", "Cornell Jeb E. Brooks School of Public Policy", 40);
    ("Healthy Futures", "College of Human Ecology", 40);
    ("History", "College of Arts and Sciences", 20);
    ("History of Art", "College of Arts and Sciences", 20);
    ("History of Capitalism", "College of Arts and Sciences", 30);
    ("Horticulture", "College of Agriculture and Life Sciences", 40);
    ("Human Development", "College of Human Ecology", 40);
    ("Inequality Studies", "College of Arts and Sciences", 30);
    ( "Infectious Disease Biology",
      "College of Agriculture and Life Sciences",
      50 );
    ( "Information Science",
      "College of Agriculture and Life Sciences, College of Arts and Sciences, \
       College of Engineering",
      60 );
    ( "International Relations",
      "Mario Einaudi Center for International Studies",
      40 );
    ( "International Development Studies",
      "College of Agriculture and Life Sciences",
      40 );
    ("International Trade and Development", "SC Johnson College of Business", 60);
    ("Italian Studies", "College of Arts and Sciences", 20);
    ("Jewish Studies", "College of Arts and Sciences", 20);
    ("Landscape Studies", "College of Agriculture and Life Sciences", 40);
    ( "Latin American Studies",
      "Mario Einaudi Center for International Studies",
      30 );
    ("Latina/o Studies", "College of Arts and Sciences", 20);
    ("Law and Society", "College of Arts and Sciences", 30);
    ("Leadership", "College of Agriculture and Life Sciences", 30);
    ( "Lesbian, Gay, Bisexual, & Transgender Studies",
      "College of Arts and Sciences",
      20 );
    ("Linguistics", "College of Arts and Sciences", 30);
    ("Marine Biology", "College of Agriculture and Life Sciences", 50);
    ("Materials Science and Engineering", "College of Engineering", 60);
    ("Mathematics", "College of Arts and Sciences", 50);
    ("Mechanical Engineering", "College of Engineering", 60);
    ("Media Studies", "College of Arts and Sciences", 30);
    ("Medieval Studies", "College of Arts and Sciences", 20);
    ("Migration Studies", "Mario Einaudi Center for International Studies", 40);
    ( "Minority, Indigenous, and Third World Studies",
      "College of Arts and Sciences",
      30 );
    ("Moral Psychology", "College of Arts and Sciences", 30);
    ("Music", "College of Arts and Sciences", 30);
    ("Near Eastern Studies", "College of Arts and Sciences", 20);
    ( "Nutrition & Health",
      "College of Agriculture and Life Sciences, College of Human Ecology",
      50 );
    ("Operations Research & Management Science", "College of Engineering", 60);
    ("Performing and Media Arts", "College of Arts and Sciences", 30);
    ("Philosophy", "College of Arts and Sciences", 20);
    ("Physics", "College of Arts and Sciences", 40);
    ("Plant Breeding", "College of Agriculture and Life Sciences", 40);
    ("Plant Sciences", "College of Agriculture and Life Sciences", 40);
    ( "Policy Analysis and Management",
      "Cornell Jeb E. Brooks School of Public Policy",
      40 );
    ("Portuguese and Brazilian Studies", "College of Arts and Sciences", 20);
    ("Psychology", "College of Arts and Sciences", 30);
    ("Public History", "College of Arts and Sciences", 20);
    ("Public Policy", "College of Arts and Sciences", 40);
    ("Public Service Studies", "College of Arts and Sciences", 30);
    ( "Real Estate",
      "Cornell Peter and Stephanie Nolan School of Hotel Administration",
      60 );
  ]

let rec take n l =
  if n <= 0 then []
  else match l with [] -> [] | x :: xs -> x :: take (n - 1) xs

let pick_major () =
  let shuffle lst = shuffle lst in
  if List.length chosen_majors >= 2 then (
    print_endline "You can't add anymore, you already have two majors";
    (chosen_majors, initial_money))
  else
    let shuffled_majors = shuffle majors in
    print_endline "Pick 3 cards and then choose your major:";
    List.iteri
      (fun i (maj, _, _) -> Printf.printf "%d: %s\n" (i + 1) maj)
      (take 3 shuffled_majors);
    print_endline "Choose your major (1-3):";
    let choice = read_int () in
    let chosen_major, _, money_bonus = List.nth shuffled_majors (choice - 1) in
    print_endline ("You have chosen: " ^ chosen_major);
    (chosen_major :: chosen_majors, initial_money + money_bonus)

let pick_minor () =
  if List.length chosen_minors >= 3 then (
    print_endline "You can't add anymore, you already have three minors";
    (chosen_minors, initial_money))
  else
    let shuffled_minors = shuffle minors in
    print_endline "Pick a card to choose your minor:";
    List.iteri
      (fun i (min, _, _) -> Printf.printf "%d: %s\n" (i + 1) min)
      (take 3 shuffled_minors);
    print_endline "Choose your minor (1-3):";
    let choice = read_int () in
    let chosen_minor, _, money_bonus = List.nth shuffled_minors (choice - 1) in
    print_endline ("You have chosen the minor: " ^ chosen_minor);
    (chosen_minor :: chosen_minors, initial_money + money_bonus)

let handle_event user =
  (* Access the user's current money *)
  let money = User.User1.get_money user in

  (* Access the user's current position *)
  let position = User.User1.get_position user in

  (* Access the user's majors *)
  let chosen_majors = User.User1.view_majors user in

  match position mod 15 with
  | 1 ->
      let dorms =
        [
          "RBG";
          "Hu Shih";
          "Mclintok";
          "Jameson";
          "High Rise";
          "Low Rise";
          "Dickson";
        ]
      in
      print_endline
        "These are your dorm options (RBG, Hu Shih, Mclintok Donolon, Jameson, \
         High Rise, Low Rise, Dickson):";
      print_endline "Type a number (1-7) to choose a dorm:";
      let dorm_choice = read_int () in
      let chosen_dorm = List.nth dorms (dorm_choice - 1) in
      let money_bonus =
        if List.mem chosen_dorm [ "RBG"; "Hu Shih"; "Mclintok Donolon" ] then 30
        else 0
      in
      print_endline
        ("Congratulations! You have been assigned to " ^ chosen_dorm ^ " dorm.");
      (chosen_majors, chosen_minors, money + money_bonus)
  | 2 ->
      let new_minors, new_money = pick_minor () in
      (chosen_majors, new_minors, new_money)
  | 3 ->
      print_endline
        "You just got your results back and you failed an important exam!";
      print_endline
        "This setback is a part of learning, but it comes with a cost.";
      print_endline
        "You lose 20 money points as you need to invest more in your studies.";
      let new_money = money - 20 in
      (chosen_majors, chosen_minors, new_money)
  | 4 ->
      print_endline "Do you want to join a club? You have two options:";
      print_endline "1. Join a fun club (100% acceptance, +5 money points)";
      print_endline
        "2. Try for a professional club (10% acceptance, +50 money points if \
         successful)";
      print_endline
        "Enter your choice (1 for fun club, 2 for professional club):";

      let club_choice = read_int () in
      let bonus_money =
        if club_choice = 1 then (
          print_endline
            "Congratulations! You've joined a fun club and made some new \
             friends!";
          print_endline "+5 money points for socializing!";
          5)
        else if club_choice = 2 then
          let chance = Random.int 10 in
          (* Random number between 0 and 9 *)
          if chance = 0 then (
            print_endline
              "Incredible! You've been accepted into a prestigious \
               professional club!";
            print_endline "+50 money points for your professional advancement!";
            50)
          else (
            print_endline
              "Unfortunately, you didn't get into the professional club this \
               time.";
            print_endline "Keep trying and don't lose hope!";
            0)
        else (
          print_endline "Invalid choice. Please enter 1 or 2.";
          0)
      in
      (chosen_majors, chosen_minors, money + bonus_money)
  | 5 ->
      print_endline "Oh no! You got frat flu and need some time to recover.";
      print_endline
        "This unfortunate event costs you 15 money points due to recovery. \
         Let's see if your roommate got sick.";
      let new_money = money - 15 in
      let final_money =
        let chance = Random.int 2 in
        if chance = 0 then (
          print_endline
            "Ugh! To make matters worse, your roommate also got sick because \
             of you!";
          print_endline
            "They are mad at you... and you lose an additional 10 points.";
          max 0 (new_money - 10))
        else (
          print_endline "Luckily, your roommate didn't get sick.";
          max 0 new_money)
      in
      (chosen_majors, chosen_minors, final_money)
  | 6 ->
      print_endline
        "Bad luck strikes! Choose a colored card to determine your fate:";
      print_endline "1. Red, 2. Blue, 3. Green, 4. Yellow, 5. Purple";
      let card_choice = read_int () in
      let event, points_lost =
        match card_choice with
        | 1 -> ("Your girlfriend/boyfriend breaks up with you", 20)
        | 2 -> ("You break your leg", 25)
        | 3 -> ("Your roommate got you sick", 15)
        | 4 -> ("Your best friend is mad at you", 10)
        | 5 -> ("You have a hangover", 5)
        | _ -> ("Confused, you pick no card.", 10)
      in
      print_endline ("Event: " ^ event);
      print_endline ("You lose " ^ string_of_int points_lost ^ " points.");
      (chosen_majors, chosen_minors, max 0 (money - points_lost))
  | 7 ->
      (* Ace prelim *)
      print_endline "Congratulations! You did so good on your prelim!";
      print_endline
        "This  achievement brings a few possible rewards. Choose one:";
      print_endline "1. Knowledge boost ";
      print_endline "2. Social acclaim (your friends think you are smart)";
      print_endline "3. Relaxation time (gain extra tiktok scrolling time)";
      let choice = read_int () in
      let reward, points_gain =
        match choice with
        | 1 -> ("Your knowledge boost will help in future exams", 30)
        | 2 -> ("Social acclaim increases your network", 20)
        | 3 -> ("You enjoy some relaxation in bed", 15)
        | _ -> ("You are content with just your achievement ", 0)
      in
      print_endline reward;
      print_endline ("You gain " ^ string_of_int points_gain ^ " points.");
      (chosen_majors, chosen_minors, money + points_gain)
  | 8 ->
      (* Failing class *)
      print_endline "Unfortunately, you failed a class, uh oh.";
      let point_loss = 25 in
      print_endline ("You lose " ^ string_of_int point_loss ^ " points.");
      print_endline
        "You have the option to retake the class next sem. There's a 50% \
         chance you'll regain half of the lost points.";
      print_endline "Do you want to retake the class? (1 for Yes, 2 for No)";
      let choice = read_int () in
      let final_points =
        if choice = 1 then
          let chance = Random.int 2 in
          if chance = 0 then (
            print_endline
              "You've successfully recovered! WOOOO half of your lost points \
               are regained.";
            money - point_loss + (point_loss / 2))
          else (
            print_endline
              "Despite your efforts, the retake didn't go as planned. No \
               additional points :(((.";
            money - point_loss)
        else (
          print_endline "You did not choose to retake the class.";
          money - point_loss)
      in
      (chosen_majors, chosen_minors, final_points)
  | 9 ->
      let new_majors, new_money = pick_major () in
      (new_majors, chosen_minors, new_money)
  | 10 ->
      print_endline "Time to get a new job!";
      print_endline "Do you want an on-campus job (1) or an internship (2)?";
      let type_job = read_int () in
      let points_gain =
        if type_job = 1 then (
          print_endline "Choose a colored card to determine your fate:";
          print_endline "1. Red, 2. Blue, 3. Green, 4. Yellow, 5. Purple";
          let card_choice = read_int () in
          let event, gain =
            match card_choice with
            | 1 -> ("You are going to be a barista at temple of zeus!", 30)
            | 2 -> ("You are going to work in the library", 20)
            | 3 -> ("You are going to work in the museum", 15)
            | 4 -> ("You will be a tour guide", 15)
            | 5 -> ("You will work in the gym", 30)
            | _ -> ("Confused, you pick no card", 0)
          in
          print_endline ("Job: " ^ event);
          print_endline ("You gain " ^ string_of_int gain ^ " points.");
          gain)
        else if type_job = 2 then (
          print_endline "How many LinkedIn connections do you have (0-500)?";
          let connections = read_int () in
          print_endline "What is your GPA (0.0-4.0)?";
          let gpa = read_float () in
          print_endline "Are either of your parents CEO of a bank (yes or no)?";
          let ceo_parent = read_line () in
          let score =
            (float_of_int connections /. 10.0 *. gpa)
            +. if ceo_parent = "yes" then 100.0 else 0.0
          in
          if score > 80.0 then (
            print_endline "Congratulations! You've secured an internship!";
            print_endline "+40 points for your career development!";
            40)
          else 0)
        else (
          print_endline "Invalid choice. You got no job.";
          0)
      in
      (chosen_majors, chosen_minors, money + points_gain)
  | 11 ->
      print_endline
        "Do you want to email administration to ask for more aid? (yes or no)";
      let email_choice = read_line () in
      let money_after_email =
        if email_choice = "yes" then
          let chance = Random.int 4 in
          if chance = 0 then (
            print_endline
              "Incredible! Aid package has been reinstated! You get 50";
            money + 50 (* Reinstating the original tuition cost *))
          else (
            print_endline
              "Unfortunately, your request for additional aid was denied.";
            money)
        else money
      in

      print_endline
        "Do you want to buy a lottery ticket for $10 to help pay for tuition? \
         (yes or no)";
      let lottery_choice = read_line () in
      let final_money =
        if lottery_choice = "yes" then
          let chance = Random.int 20 in
          (* 5% chance *)
          if chance = 0 then (
            print_endline "You won the lottery! Your tuition is covered!";
            money_after_email + 50 - 10
            (* Recovering the tuition cost, minus the lottery ticket cost *))
          else (
            print_endline "Sadly, you didn't win anything in the lottery.";
            money_after_email - 10)
        else money_after_email
      in
      (chosen_majors, chosen_minors, final_money)
  | 12 ->
      print_endline
        "It's time to rush! Do you want to join a sorority or a fraternity? (1 \
         for Sorority, 2 for Fraternity)";
      let rush_choice = read_int () in

      if rush_choice = 1 then (
        print_endline
          "Choose a sorority to rush: 1. APhi, 2. DG, 3. Theta, 4. KKG, 5. \
           AZD, 6. SDT, 7. Pi Phi, 8. Tri Delt, 9. KD, 10. AXO";
        let sorority_choice = read_int () in
        let sorority_benefit =
          match sorority_choice with
          | 1 -> ("APhi", 10)
          | 2 -> ("DG", 15)
          | 3 -> ("Theta", 20)
          | 4 -> ("KKG", 25)
          | 5 -> ("AZD", 30)
          | 6 -> ("SDT", 35)
          | 7 -> ("Pi Phi", 40)
          | 8 -> ("Tri Delt", 45)
          | 9 -> ("KD", 50)
          | 10 -> ("AXO", 55)
          | _ -> ("Unknown", 0)
        in
        print_endline ("You decided to rush " ^ fst sorority_benefit ^ ".");

        (* Check for executive board position *)
        if Random.int 2 = 0 then (
          let eboard_positions =
            [ "President"; "Vice President"; "Treasurer"; "Secretary" ]
          in
          let eboard_index = Random.int (List.length eboard_positions) in
          let eboard_position = List.nth eboard_positions eboard_index in
          print_endline
            ("Congratulations! You've been selected as the " ^ eboard_position
           ^ " of " ^ fst sorority_benefit ^ ".");
          (chosen_majors, chosen_minors, money + snd sorority_benefit + 20))
        else (chosen_majors, chosen_minors, money + snd sorority_benefit))
      else (
        print_endline
          "Choose a fraternity to rush: 1. SigChi, 2. SigPhi, 3. Sig Pi, 4. \
           AD, 5. ZBT, 6. DX, 7. Chi Phi, 8. Lambda, 9. Chi Psi, 10. PSK, 11. \
           Pike";
        let fraternity_choice = read_int () in
        let fraternity_benefit =
          match fraternity_choice with
          | 1 -> ("SigChi", 10)
          | 2 -> ("SigPhi", 15)
          | 3 -> ("Sig Pi", 20)
          | 4 -> ("AD", 25)
          | 5 -> ("ZBT", 30)
          | 6 -> ("DX", 35)
          | 7 -> ("Chi Phi", 40)
          | 8 -> ("Lambda", 45)
          | 9 -> ("Chi Psi", 50)
          | 10 -> ("PSK", 55)
          | 11 -> ("Pike", 60)
          | _ -> ("Unknown", 0)
        in
        print_endline
          ("You decided to rush " ^ fst fraternity_benefit ^ " and gained "
          ^ string_of_int (snd fraternity_benefit)
          ^ " points.");
        (chosen_majors, chosen_minors, money + snd fraternity_benefit))
  | 13 -> (
      print_endline "OH NO you wake up to a fire in your dorm! What do you do?";
      print_endline "1. Grab your important stuff and leave";
      print_endline "2. Immediately leave and you do not take anything";
      print_endline "3. Check on your dorm neighbors before leaving";
      let fire_choice = read_int () in
      match fire_choice with
      | 1 ->
          print_endline "You grab your phone, laptop and a few important items.";
          print_endline "While you saved your things, it took extra time.";
          let is_safe = Random.int 2 = 0 in
          if is_safe then (
            print_endline
              "That was a risky choice but luckily you managed to get out \
               safely.";
            print_endline
              "Minus 10 points for the risk but keep your belongings.";
            (chosen_majors, chosen_minors, money - 10))
          else (
            print_endline
              "Uh oh, you're caught in the smoke and need medical attention.";
            print_endline
              "You lose 30 points and some of your things are damaged :((((";
            (chosen_majors, chosen_minors, money - 30))
      | 2 ->
          print_endline "You prioritized your safety and left the dorm.";
          print_endline "You're safe but you've left all your valuable stuff!";
          print_endline "You get 20 points for staying safe.";
          (chosen_majors, chosen_minors, money + 20)
      | 3 ->
          print_endline "You bravely check on your neighbors!";
          let is_hero = Random.int 2 = 0 in
          if is_hero then (
            print_endline
              "Your bravery saves 4 lives! You go on news as a hero.";
            print_endline "You gain 60 points for courageous action.";
            (chosen_majors, chosen_minors, money + 60))
          else (
            print_endline "While your intentions were good, you caught on fire.";
            print_endline "You're rescued but end up in the hospital.";
            print_endline
              "You lose 20 points but got respect from your friends.";
            (chosen_majors, chosen_minors, money - 20))
      | _ ->
          print_endline
            "Confused and panicked, you struggle to make a decision.";
          print_endline "You lose 15 points due to the trauma of the fire.";
          (chosen_majors, chosen_minors, money - 15))
  | 14 -> (
      let locations = [ "at the house"; "IBC"; "boatyard"; "Level B" ] in
      let location = List.nth locations (Random.int (List.length locations)) in
      print_endline "The formal season is here!";
      print_endline "1. Ask someone out to formal";
      print_endline "2. Go to club formal with your friend";
      print_endline "3. Go to a sorority/fraternity formal with a date";
      print_endline "4. Go to a sorority/fraternity formal with a friend";
      let formal_choice = read_int () in

      match formal_choice with
      | 1 ->
          let ask_outcome = Random.int 2 in
          if ask_outcome = 0 then (
            print_endline
              "You asked someone out... they said YES! WOOOO PARTAYYYYYY!";
            print_endline ("The formal is at " ^ location ^ ".");
            print_endline
              "You both had an AMAZING time. +20 points for making friends!";
            (chosen_majors, chosen_minors, money + 20))
          else (
            print_endline
              "You asked someone out... they said no! Embarrassing...";
            print_endline "You go alone but still have an ok time";
            print_endline "You gain 5 points.";
            (chosen_majors, chosen_minors, money + 5))
      | 2 ->
          print_endline
            ("You decide to go to a club formal with friends at " ^ location
           ^ ".");
          print_endline
            "It turns out to be super super fun. +15 points for a wonderful \
             time!";
          (chosen_majors, chosen_minors, money + 15)
      | 3 ->
          print_endline
            ("You go to a sorority/fraternity formal with a date at " ^ location
           ^ ".");
          print_endline
            "You and your date kiss! What a successful and memorable evening. \
             +25 points!";
          (chosen_majors, chosen_minors, money + 35)
      | 4 ->
          print_endline
            ("You hit up a sorority/fraternity formal with friends at "
           ^ location ^ ".");
          print_endline "+20 points for a fun time!";
          (chosen_majors, chosen_minors, money + 20)
      | _ ->
          print_endline
            "You're not sure what to do, so you end up binge-watching shows \
             instead (kinda lame)";
          (chosen_majors, chosen_minors, money + 5))
  | 15 ->
      (* Loco Trivia  *)
      print_endline "It's trivia night at Loco!!!!!!!";
      print_endline
        "You'll answer three trivia questions. Each correct answer gives you \
         15 points!";
      let trivia_questions =
        [
          ("Who was the third U.S. president?", "Thomas Jefferson");
          ("What gas do plants absorb from the atmosphere?", "Carbon Dioxide");
          ("Who started Amazon?", "Jeff Bezos");
        ]
      in

      let rec ask_questions questions points =
        match questions with
        | [] -> points
        | (q, a) :: t ->
            print_endline q;
            let answer = read_line () in
            if String.lowercase_ascii answer = String.lowercase_ascii a then (
              print_endline "Correct yay!";
              ask_questions t (points + 15))
            else (
              print_endline ("Wrong! The correct answer is " ^ a);
              ask_questions t points)
      in

      let total = ask_questions trivia_questions 0 in
      print_endline
        ("You scored a total of " ^ string_of_int total
       ^ " points at trivia night!");
      (chosen_majors, chosen_minors, money + total)
  | _ -> (chosen_majors, chosen_minors, money)
(* | x ->
   let max_spots = 15 in
    let new_position = ((position - 1 + x) mod max_spots) + 1 in
    let new_user =
      Game.User.User1.update_profile
        (Game.User.User1.get_name user)
        money new_position
        (Game.User.User1.get_sum_of_rolls user)
        chosen_majors chosen_minors
    in
    handle_event new_user *)
