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

(* Lets user choose a country to play as. Continues asking until a valid
 * country is entered. *)
let rec gui_get_country () =
    Gui.print_to_cmd "Which country would you like to play?\n";
    try Game_utils.enter_country (get_input ()) with
    | Invalid_Country s ->
        Gui.print_to_cmd (s ^ " is not a valid force.\n");
        Gui.print_to_cmd "Try again.\n";
        gui_get_country ()

let print_game_display ct : unit =
    Gui.print_to_display Gui.game_info_display 
    ("Country played: " ^ (Board.String.string_of_country ct) ^ "\n" ^
    "Supply centers to win: " ^ (string_of_int 18) ^ "\n" ^
    "Variant: Normal\n")



let run_game () =
    
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
    let ai_cts = List.filter (fun c -> c != user_country) 
            [England; France; Germany; Russia; Turkey; Austria; Italy] in
    Init.init_AIs game_board ai_cts;

    print_game_display user_country;

    (* begin main game loop until winner declared 
    while Board.is_won game_board.players do
        Gui.print_to_cmd "Enter your orders.\n" ^*)



    ()

