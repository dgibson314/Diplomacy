type country = England | France | Germany | Russia | Italy |
               Turkey | Austria | Neutral
type climate = Inland | Coastal | Ocean
type province = {
    name : string;
    supply : bool;
    homeland : country;
    climate : climate;
    mutable held_by : country;
    mutable occupied : bool;
    mutable hold_strength : int
}
type branch = Army | Fleet
type state = Resolved | Unresolved | Guessing
type resolution = Fails | Succeeds
type order = Attack of province list |
             Support of (force * order) | (* order should only be Attack or Hold*)
             Convoy of force * (province * province) |
             Hold | Void
and
force = {
    branch : branch;
    country : country;
    mutable occupies : province;
    mutable attack_strength : int;
    mutable order : order;
    mutable order_state : state
}

type player = {
    country : country;
    mutable supply_centers : province list;
    mutable forces : force list
}
(* TODO:
    * should this include a list of players? *)
type board = {
    provinces : province list;
    mutable forces : force list;
    adjacents : (province * province) list;
    mutable players : player list
}

type phase = Move | Retreat
type season = Spring of phase | Fall of phase | Adjustment

exception NotAdjacent
exception NotSupplyCenter

let gen_force p ?(fleet = false) =
    if p.supply then
        let f = {branch = Fleet;
                 country = p.homeland;
                 occupies = p;
                 attack_strength = 0;
                 order = Void;
                 order_state = Unresolved} in
        if p.climate = Coastal && fleet then f
        else {f with branch = Army}
    else
        raise NotSupplyCenter

(* is_adjacent tests whether a province list is a valid path, i.e. whether
 * each province in the list is adjacent to it's predecessor and the next one.
 * Does this by seeing if each pair of provinces in the list are a tuple in the
 * game board's adjacency list. *)
let rec is_adjacent bd path =
    let rec adj_search lst p1 p2 =
        match lst with
        | [] -> false
        | (h1, h2) :: t ->
            if (h1 = p1 && h2 = p2) || (h1 = p2 && h2 = p1) then
                true
            else
                adj_search t p1 p2
    in
    match path with
    | [] -> true
    | h::m::t -> 
        if (adj_search bd.adjacents h m) then
            (is_adjacent bd (m::t))
        else false
    | _::[] -> true

let rmv_dupl lst =
    List.fold_left (fun acc p -> if List.mem p acc then acc else p::acc) [] lst

let valid_order bd fc =
    match fc.order with
    | Attack(path) ->
        (* if no convoying goin on *)
        if ((List.length path) = 1) then
            is_adjacent bd (fc.occupies::path)
        else if fc.branch = Army then
            let rec convoy_helper = function
                | p::[] -> p.climate = Coastal
                | h::t -> h.climate = Ocean && h.occupied && convoy_helper t
                | _ -> failwith "valid_order, Attack"
            in
            let clean_path = rmv_dupl path in
            is_adjacent bd clean_path && convoy_helper clean_path
        else false
    | Support(fc', _) -> failwith "valid_order, Support"
    | Convoy(fc', (p1, p2)) ->
        is_adjacent bd [fc.occupies; p1] &&
        is_adjacent bd [fc.occupies; p2] &&
        (match p1.climate, p2.climate with
        | Ocean, Ocean -> p1.occupied && p2.occupied
        | Ocean, Coastal -> p1.occupied
        | Coastal, Ocean -> p2.occupied
        | Coastal, Coastal -> true
        | _, _ -> false)     (* if inland army tries getting convoyed *)
    | _ -> true

let get_supplycenters bd ct =
    List.fold_left 
        (fun acc p -> if p.supply && p.held_by = ct then p :: acc else acc)
        []
        bd.provinces;;

(* explicit 'cause type checker is fucking crazy *)
(* probably my fault for reusing record field names though :( *)
let get_forces (bd : board) ct : force list=
    List.fold_left
        (fun acc (f : force) -> if f.country = ct then f::acc else acc)
        []
        bd.forces


let is_won plst =
    try Some (List.find (fun p -> (List.length p.supply_centers) >= 18) plst)
    with Not_found -> None


module String =
struct

    (**** TO STRING FUNCTIONALITY ****)

    let abrev_of_country = function
        | England -> "E"
        | France -> "F"
        | Germany -> "G"
        | Russia -> "R"
        | Turkey -> "T"
        | Austria -> "A"
        | Italy -> "I"
        | Neutral -> "N"

    let string_of_country = function
        | England -> "England"
        | France -> "France"
        | Germany -> "Germany"
        | Russia -> "Russia"
        | Turkey -> "Turkey"
        | Austria -> "Austria"
        | Italy -> "Italy"
        | Neutral -> "Neutral"

    let string_of_branch = function
        | Army -> "Army"
        | Fleet -> "Fleet"

    let abrev_of_branch = function
        | Army -> "A"
        | Fleet -> "F"
    
    (* Army PIC [E] *)
    let string_of_force f =
        (string_of_branch f.branch) ^ " " ^
        f.occupies.name ^ " [" ^
        (abrev_of_country f.country) ^ "]\n"

    (* STRING RULES:
        * Attack ->
        * Support ==
        * Convoy <>
        * Hold ||
     * A BRE [F] -> PAR
     * A BRE [F] == A PAR [F] -> PIC
     * F MAO [E] <> A BRE [F] -> BRE -> LON *)
    let rec string_of_order f =
        match f.order with
        | Attack(path) ->
            (string_of_force f) ^ " " ^
            (List.fold_left (fun s p -> "-> " ^ p.name ^ s) "" path)
        | Support(f', cm) ->
            (string_of_force f) ^ " == " ^
            (match cm with
             | Attack(path) -> string_of_order {f' with order = cm}
             | _ -> string_of_force f')
        | Convoy(f', (p1, p2)) ->
            (string_of_force f) ^ " <> " ^
            (string_of_force f') ^ " -> " ^ p1.name ^ " -> " ^ p2.name
        | Hold ->
            (string_of_force f) ^ " ||"
        | Void ->
            (string_of_force f) ^ " Void"

    let string_of_forces lst =
        List.fold_left (fun s f -> (string_of_force f) ^ s) "" lst

    let string_of_provs lst =
        List.fold_left (fun s p -> p.name ^ "\n" ^ s) "" lst

    let cheat_forces lst =
        List.fold_left (fun s f -> (string_of_force f) ^ ":\n" ^
                                   ("\tOrder: ")^(string_of_order f) ^ "\n")
                        "" lst

    let string_of_board bd =
        "Provinces:\n" ^ (string_of_provs bd.provinces) ^
        "Forces:\n" ^ (string_of_forces bd.forces)

    let cheat_board bd =
        "Provinces:\n" ^ (string_of_provs bd.provinces) ^
        "Forces:\n" ^ (cheat_forces bd.forces)
        

    (**** FROM STRING FUNCTIONALITY ****)
    
    let prov_of_string bd id =
        List.find (fun p -> (String.uppercase p.name) = id) bd.provinces

    let provs_of_strings bd lst =
        List.fold_left (fun p s -> (prov_of_string bd s)::p) [] lst

    let branch_of_string str =
        match str with
        | "A" | "Army" | "ARMY" | "army"  -> Army
        | "F" | "Fleet"| "FLEET"| "fleet" -> Fleet
        | _ -> failwith "Invalid string"

end 
