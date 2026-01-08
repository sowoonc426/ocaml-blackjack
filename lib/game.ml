open Player
open Hand
open Deck

let sleep seconds = Unix.sleepf seconds
let separator = String.make 40 '-'

let print_separator ?(title = "") output =
  output (title ^ "\n" ^ separator ^ "\n\n")

type input_source = unit -> string
type output_source = string -> unit

type input =
  | Hit
  | Stand
  | Split
  | DoubleDown
  | Shadow
  | Flip
  | Surrender
  | Invalid

let next_input player hand is_first_turn input output delay =
  output "Do you want to ";
  let options = ref [ "(H)it"; "(S)tand" ] in
  if is_first_turn then options := !options @ [ "S(u)rrender" ];
  if Player.can_special player hand && is_first_turn then
    options := !options @ [ "$(D)ouble down$"; "$(F)lip$"; "$Sh(a)dow$" ];
  if Player.can_split player hand && is_first_turn then
    options := !options @ [ "$S(p)lit$" ];
  output ("[ " ^ String.concat " / " !options ^ " ]? ");
  let input_string = String.trim (String.lowercase_ascii (input ())) in
  match input_string with
  | "h" -> Hit
  | "s" -> Stand
  | "p" -> Split
  | "d" -> DoubleDown
  | "a" -> Shadow
  | "f" -> Flip
  | "u" -> Surrender
  | _ -> Invalid

let rec wait_for_enter input output =
  output "Press enter to continue...\n";
  match input () with
  | "" -> ()
  | _ -> wait_for_enter input output

let rec input_int prompt input output =
  output prompt;
  match int_of_string_opt (input ()) with
  | Some x -> x
  | None -> input_int prompt input output

let rec input_int_options prompt options input output =
  let x = input_int prompt input output in
  if List.mem x options then x
  else input_int_options prompt options input output

let deal_card deck =
  try Deck.next_card deck
  with Deck.Empty ->
    Deck.reset deck;
    Deck.shuffle deck;
    Deck.next_card deck

let deal deck hand =
  let card = deal_card deck in
  Hand.add_card hand card;
  card

let print_hand output hand =
  let strings = List.map Card.string hand.cards in
  let string = String.concat " " strings in
  output (string ^ "\n")

let hand_turn_skip hand (output : output_source) =
  let value = Hand.value hand in
  if value < 21 then false
  else (
    if value = 21 then output "\tBlackjack! You win, and your turn is over.\n"
    else output "\tBust! Your turn is over.\n";
    true)

let rec player_turn_hand dealer player deck hand input output delay =
  let hand_num = player.hands.currHand in
  output ("Hand #" ^ string_of_int (hand_num + 1) ^ "'s Turn: \n");

  let rec input_loop is_first_turn =
    output "\n";
    print_hand output hand;
    if hand_turn_skip hand output then ()
    else
      match next_input player hand is_first_turn input output delay with
      | Hit ->
          output "\n";
          output "Hit!\n";
          sleep delay;
          let card = deal deck hand in
          output ("Got: " ^ Card.string card ^ "\n");
          sleep delay;
          input_loop false
      | Stand -> ()
      | Split ->
          if not (Player.can_split player hand && is_first_turn) then
            input_loop is_first_turn
          else
            let new_hand1, new_hand2 = Player.split player hand in
            let new_card1 = deal deck new_hand1 in
            let new_card2 = deal deck new_hand2 in
            let new_wager = hand.wager in
            output "\nDeck split!";
            sleep delay;
            output ("\n\tHand #" ^ string_of_int (hand_num + 1) ^ " hit: ");
            sleep delay;
            output (Card.string new_card1);
            sleep delay;
            output ("\n\tHand #" ^ string_of_int (hand_num + 2) ^ " hit: ");
            sleep delay;
            output (Card.string new_card2);
            sleep delay;
            output
              ("\nWagered an additional $" ^ string_of_int new_wager ^ ".\n");
            input_loop true
      | DoubleDown ->
          if not (Player.can_special player hand && is_first_turn) then
            input_loop is_first_turn
          else
            let new_wager = hand.wager in
            player.money <- player.money - new_wager;
            hand.wager <- hand.wager + new_wager;
            let new_card = deal deck hand in
            output "\nDoubled down!";
            sleep delay;
            output "\n\tHit: ";
            sleep delay;
            output (Card.string new_card);
            sleep delay;
            output
              ("\nWagered an additional $" ^ string_of_int new_wager ^ ".\n");
            output "\n";
            print_hand output hand;
            ()
      | Shadow ->
          if not (Player.can_special player hand && is_first_turn) then
            input_loop is_first_turn
          else (
            output "\nShadowing...\n";
            let shadow_card_num =
              input_int_options "Shadow which card into a new hand? [ 1 / 2 ] "
                [ 1; 2 ] input output
            in
            let shadow_card, new_hand =
              Player.shadow player hand shadow_card_num
            in
            output ("\n" ^ Card.string shadow_card ^ " shadowed!\n");
            sleep delay;
            output "\tHit: ";
            sleep delay;
            let new_card = deal deck new_hand in
            output (Card.string new_card);
            sleep delay;
            output "\n\tNew hand: ";
            sleep delay;
            print_hand output new_hand;
            sleep delay;
            input_loop false)
      | Flip ->
          if not (Player.can_special player hand && is_first_turn) then
            input_loop is_first_turn
          else (
            output "\nFlipping...\n";
            let flip_card_num =
              input_int_options
                "Flip which card with the dealer's face-up card? [ 1 / 2 ] "
                [ 1; 2 ] input output
            in
            Hand.swap_card hand (flip_card_num - 1) dealer 0;
            sleep delay;
            output "\nFlipped!\n";
            sleep delay;
            let new_wager = hand.wager in
            hand.wager <- hand.wager + new_wager;
            player.money <- player.money - new_wager;
            output ("Wagered an additional $" ^ string_of_int new_wager ^ ".\n");
            sleep delay;
            input_loop false)
      | Surrender ->
          if not is_first_turn then input_loop is_first_turn
          else
            let amount = Player.surrender player hand in
            output "\nYou surrendered.\n";
            output
              ("This hand is removed from the game. You receive $"
             ^ string_of_int amount ^ " back.\n")
      | Invalid -> input_loop is_first_turn
  in

  input_loop true;
  print_separator output

let dealer_turn dealer deck output delay =
  output "Dealer's Turn:\n\n";
  let rec fill_dealer_cards () =
    sleep delay;
    output "Dealer has: ";
    print_hand output dealer;
    sleep delay;
    let value = Hand.value dealer in
    if value > 17 then ()
    else if value = 17 && not (Card.contains_high_ace dealer.cards) then ()
    else
      let card = deal deck dealer in
      output ("Hit: " ^ Card.string card ^ "\n\n");
      fill_dealer_cards ()
  in
  fill_dealer_cards ();
  output "End round.\n";
  print_separator output

let init_player player deck input output =
  let rec input_wager () =
    let wager = input_int "Input wager: $" input output in
    if wager <= 0 then input_wager ()
    else if wager > player.money then (
      output "Wager too large. Please try again.\n";
      input_wager ())
    else wager
  in
  Player.reset player (input_wager ());
  let add_card () =
    let card = deal_card deck in
    Player.add_card_to_hand player card
  in
  add_card ();
  add_card ()

let init_dealer deck output =
  let dealer = Hand.empty_unwagered () in
  let _ = deal deck dealer in
  let card2 = deal deck dealer in
  output ("Dealer has: " ^ Card.string card2 ^ " ██\n");
  print_separator output;
  dealer

type result_type =
  | Tie
  | BlackjackWin
  | Win
  | Loss

let decide_hand_result dealer player hand output delay =
  let hand_num = player.hands.currHand in
  output ("\nHand #" ^ string_of_int (hand_num + 1) ^ ": ");
  print_hand output hand;
  sleep delay;
  let dealer_value = Hand.value dealer in
  let hand_value = Hand.value hand in
  let blackjack = 21 in
  let output_result msg =
    output
      ("\tYou have " ^ string_of_int hand_value ^ ", dealer has "
     ^ string_of_int dealer_value ^ ", so: \n");
    sleep delay;
    output msg;
    sleep delay
  in
  if hand_value > blackjack then (
    output_result "Bust! You lose ):\n";
    Loss)
  else if hand_value = dealer_value then (
    output_result "Tie!\n";
    Tie)
  else if hand_value = blackjack then (
    output_result "Blackjack! You win!\n";
    BlackjackWin)
  else if dealer_value > blackjack then (
    output_result "Dealer bust! You win!\n";
    Win)
  else if hand_value < dealer_value then (
    output_result "You lose ):\n";
    Loss)
  else (
    output_result "You win!\n";
    Win)

let apportion_hand_winnings player hand result output =
  let wager = hand.wager in
  let winnings =
    match result with
    | BlackjackWin -> Int.of_float (Float.of_int wager *. 2.5)
    | Win -> wager * 2
    | Tie -> wager
    | Loss -> 0
  in
  player.money <- player.money + winnings;
  let diff = winnings - wager in
  if diff = 0 then ()
  else if diff < 0 then output ("You lost $" ^ string_of_int wager ^ "\n")
  else output ("You won $" ^ string_of_int diff ^ "\n")

let judge_game_end player = player.money <= 0

let rec game_loop ?(recursive = true) round player deck (input : input_source)
    (output : output_source) delay =
  let recurse () =
    if recursive then (
      (wait_for_enter input output;
       game_loop ~recursive (round + 1) player deck input output delay)
      [@coverage off])
  in

  print_separator output
    ~title:
      ("\nROUND " ^ string_of_int round ^ "\nYou have $"
     ^ string_of_int player.money);
  let dealer = init_dealer deck output in
  init_player player deck input output;

  let single_hand_turn hand =
    player_turn_hand dealer player deck hand input output delay
  in
  HandGroup.map_over_hands player.hands single_hand_turn;

  if HandGroup.is_empty player.hands then (
    output "Round over. No hands remaining.\n\n";
    recurse ())
  else (
    dealer_turn dealer deck output delay;
    let single_hand_result hand =
      let result = decide_hand_result dealer player hand output delay in
      apportion_hand_winnings player hand result output
    in
    output "RESULTS\n";
    HandGroup.map_over_hands player.hands single_hand_result;
    print_separator output;
    if judge_game_end player then (
      output "GAME OVER\n";
      output
        ("You ran out of money. Final amount: $" ^ string_of_int player.money
       ^ "\n"))
    else recurse ())

let print_rules name input output =
  let print_section title items =
    output ("\n\t" ^ title ^ ":\n");
    let print_item item = output ("\t\t* " ^ item ^ "\n") in
    ignore (List.map print_item items)
  in
  output "\n---- BLACK J.A.C.K. ----\n";
  output ("\nWelcome, " ^ name ^ "!\n");
  print_section "Rules"
    [
      "Beat the dealer's hand without exceeding 21 points.";
      "Number cards are worth their number.";
      "Face cards are worth 10.";
    ];
  print_section "Your moves"
    [
      "Hit: Draw a card.";
      "Stand: End your hand's turn.";
      "Surrender: Give up and get half your bet back. (Round start only)";
    ];
  print_section "Special moves [$ doubles your bet!! $]"
    [
      "Double down: Draw one more card, and end your hand's turn.";
      "Flip: Swap cards with the dealer's face-up card.";
      "Shadow: 'Shadow' one of your cards, duplicating it into a new hand.";
      "Split: Break your hand into two, drawing one more card per hand.";
    ];
  output "\n";
  wait_for_enter input output

let start_game ?(recursive = true) ?(broken = false) money
    (input : input_source) (output : output_source) delay =
  let deck = if broken then Deck.init_broken () else Deck.init () in
  Deck.shuffle deck;
  output "Please enter your name: ";
  let player_name = input () in
  let player = Player.init player_name money in
  print_rules player_name input output;
  game_loop ~recursive 1 player deck input output delay
