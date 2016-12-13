open Board
open Init

exception Invalid_Country of string
exception Invalid_Force of string

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
    let split = Str.split (Str.regexp " +") in
    let tokens = split str in
    match (List.length tokens) with
    | 0 -> failwith "No force entered"
    | 1 -> failwith "Incorrect force entry"
    | 2 ->
        (let branch = List.hd tokens in
        let province = List.nth tokens 1 in
        try
            let find_force (f : force) = 
                    f.country = pl.country &&
                    f.occupies.name = province &&
                    Board.String.string_of_branch f.branch = branch in
            List.find find_force pl.forces
        with Not_found -> failwith "No force found")
    | _ -> raise (Invalid_Force str) 
