open Board
open Init


(*-------------------------------------------------------------------*)
(*------------------------ List Helpers -----------------------------*)
(*-------------------------------------------------------------------*)

(* Drops the first n elements of lst *)
let rec drop lst n =
    if n < 0 then failwith "Invalid argument: List drop"
    else
    match lst with
    | [] -> []
    | h::t ->
        if n = 0 then lst
        else drop t (n-1)

let rmv_arrow lst =
    List.filter (fun s -> not (s = "->")) lst


(*-------------------------------------------------------------------*)
(*----------------- Entering Forces and Orders ----------------------*)
(*-------------------------------------------------------------------*)

exception Invalid_Country of string
exception Invalid_Force of string
exception Invalid_Order of string

let split = Str.split (Str.regexp " +")

let enter_country str =
    match str with
    | "England" -> England
    | "France" -> France
    | "Germany" -> Germany
    | "Russia" -> Russia
    | "Austria" -> Austria
    | "Italy" -> Italy
    | "Turkey" -> Turkey
    | "Random" ->
        Random.self_init ();
        let index = Random.int 7 in
        let countries = [England; France; Germany; Austria; Turkey; Italy] in
        List.nth countries index
    | _ -> raise (Invalid_Country str)

(* Let's assume that the gui works, and a player can successfully submit an
 * force string. *)
let enter_force bd (pl : player) str =
    let tokens = split str in
    match (List.length tokens) with
    | 0 -> raise (Invalid_Force "No force entered")
    | 2 ->
        (let branch = List.hd tokens in
        let province = (List.nth tokens 1) in
        try
            let find_force (f : force) = 
                    f.country = pl.country &&
                    f.occupies.name = province &&
                    Board.String.abrev_of_branch f.branch = branch in
            List.find find_force pl.forces
        with Not_found -> raise (Invalid_Force str))
    | _ -> raise (Invalid_Force "Incorrect force entry") 


(* Entering orders rules:
    * F BRE -> NOR    attack
    * A PAR ||        hold
    * F ENG <> A BRE -> LON  convoy
    * F NOR <> A BRE -> ENG -> NOW convoy from sea to other province *)
let enter_order bd (pl : player) str =
    let tokens = split str in
    (* get just the force string to be read by enter_force *)
    let force_str = (List.hd tokens) ^ " " ^ (List.nth tokens 1) in
    let selected_force = enter_force bd pl force_str in
    match (List.nth tokens 2) with
    | "->" ->
        let str_move_path = rmv_arrow (drop tokens 3) in
        let move_path = Board.String.provs_of_strings bd str_move_path in
        selected_force.order <- Attack(move_path)
    | "||" -> selected_force.order <- Hold
    | "<>" ->
        let convoy_lst = rmv_arrow (drop tokens 3) in
        let convoyed_force_str = 
            (List.hd convoy_lst) ^ " " ^ (List.nth convoy_lst 1) in
        let convoyed_force = enter_force bd pl convoyed_force_str in
        if convoyed_force.branch = Army then
            let convoy_path = drop convoy_lst 2 in
            (match convoy_path with
            | p::[] -> (try
                let pickup = convoyed_force.occupies in
                let dropoff = Board.String.prov_of_string bd p in
                selected_force.order <- Convoy(convoyed_force, (pickup, dropoff))
                with Not_found -> raise (Invalid_Order "Convoy"))
            | p::d::[] -> (try
                let pickup = Board.String.prov_of_string bd p in
                let dropoff = Board.String.prov_of_string bd d in
                selected_force.order <- Convoy(convoyed_force, (pickup, dropoff))
                with Not_found -> raise (Invalid_Order "Convoy"))
            | _ -> raise (Invalid_Order "Convoy"))
        else raise (Invalid_Order "Convoy")

            
