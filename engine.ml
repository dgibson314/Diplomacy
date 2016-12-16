open Board
open Async.Std
open Game_utils


let get_input () =
    let wait_lock = ref (Mutex.create ()) in
    let cmd_input_str = ref "" in
    Mutex.unlock (!wait_lock);
    Gui.readline wait_lock cmd_input_str;
    Mutex.lock (!wait_lock);
    Mutex.unlock (!wait_lock);
    !cmd_input_str

let rec gui_get_force (bd : board) (pl : player) =
    Gui.print_to_cmd "Enter a force:\n";
    try Game_utils.enter_force bd pl (get_input ()) with
    | Invalid_Force f ->
        Gui.print_to_cmd (f ^ ". Try again.\n");
        gui_get_force bd pl


(* Lets user choose a country to play as. Continues asking until a valid
 * country is entered. *)
let rec gui_get_country () =
    Gui.print_to_cmd "Which country would you like to play?\n";
    try Game_utils.enter_country (get_input ()) with
    | Invalid_Country s ->
        Gui.print_to_cmd (s ^ " is not a valid country.\n");
        Gui.print_to_cmd "Try again.\n";
        gui_get_country ()

let rec gui_get_order bd pl =
    Gui.print_to_cmd "Enter your order:\n";
    try 
        Game_utils.enter_order bd pl (get_input ());
        Gui.clear_cmd ()
    with 
    | Invalid_Order s ->
        Gui.print_to_cmd "Woops... Error! Try again...\n";
        gui_get_order bd pl 

(*let print_game_display ct : unit =
    Gui.print_to_display Gui.game_info_display 
    ("Country played: " ^ (Board.String.string_of_country ct) ^ "\n" ^
    "Supply centers to win: " ^ (string_of_int 18) ^ "\n" ^
    "Variant: Normal\n")*)



let run_game () =
   
    Gui.print_to_cmd "hello"; 
    (* Initialization:
        * initialize board
        * get the country user would like to play
        * initialize the AI's *)
    (* TODO:
        * how can I accomadate having different AI styles (randbot vs dumbot)
            each control a different country. Sys args, then Threads *)
    let game_board = Init.init_forces (Init.init_board ()) in

    Gui.print_to_cmd "My (pretty terrible) implementation of Diplomacy!\n";

    let user_country = gui_get_country () in
    Init.init_player game_board user_country;
    let user_player = List.hd game_board.players in
    let ai_cts = List.filter (fun c -> c != user_country) 
            [England; France; Germany; Russia; Turkey; Austria; Italy] in
    Init.init_AIs game_board ai_cts;

    (* begin main game loop until winner declared 
    while Board.is_won game_board.players do
        Gui.print_to_cmd "Enter your orders.\n" ^*)
    while Board.is_won game_board.players = None do
        gui_get_order game_board user_player;
        Gui.print_to_cmd (Board.String.string_of_orders user_player.forces)
    done;
        



    ()

