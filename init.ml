open Board
open Board.String

(* ENGLAND *)
let cly = {
    name = "CLY";
    supply = false;
    homeland = England;
    climate = Coastal;
    held_by = England;
    occupied = false;
    hold_strength = 1
}
let edi = {
    name = "EDI";
    supply = true;
    homeland = England;
    climate = Coastal;
    held_by = England;
    occupied = false;
    hold_strength = 1;
}
let yor = {
    name = "YOR";
    supply = false;
    homeland = England;
    climate = Coastal;
    held_by = England;
    occupied = false;
    hold_strength = 1;
}
let lon = {
    name = "LON";
    supply = true;
    homeland = England;
    climate = Coastal;
    held_by = England;
    occupied = false;
    hold_strength = 1;
}
let wal = {
    name = "WAL";
    supply = false;
    homeland = England;
    climate = Coastal;
    held_by = England;
    occupied = false;
    hold_strength = 1;
}
let lvp = {
    name = "LVP";
    supply = true;
    homeland = England;
    climate = Coastal;
    held_by = England;
    occupied = false;
    hold_strength = 1;
}

                         

let init_board () =
    {provinces = [
        (* ENGLAND *)
        cly; edi; yor; lon; wal; lvp;
    ];
    forces = [];
    adjacents = [
        (* ENGLAND *)
        (cly, edi); (cly, yor); (cly, lvp);
        (edi, yor);
        (lon, yor); (lon, wal);
        (wal, yor); (wal, lvp);
    ];
    players = []
    }
;;


let init_forces bd =
    let sc_list = List.filter (fun p -> p.supply && p.homeland != Neutral)
                  bd.provinces in
    let fl_list = [edi; lon;] in
    let gen_force_lst lst =
        List.fold_left
            (fun l p ->
                if List.exists (fun f -> f = p) fl_list then
                    gen_force p ~fleet:true :: l
                else
                    gen_force p ~fleet:false :: l)
            []
            lst
    in
    {bd with forces = gen_force_lst sc_list}

let init_player bd ct =
    (* create the player *)
    let pl =
       {country = ct;
        supply_centers = Board.get_supplycenters bd ct;
        forces = Board.get_forces bd ct} in
    (* make sure player doesn't already exist *)
    if List.exists (fun p -> p.country = ct) bd.players then
        failwith "Player already exists"
    else
        bd.players <- pl :: bd.players


let init_AIs bd lst =
    let ai_lst =
        List.fold_left
            (fun acc c ->
                let (ai : player) =
                    {country = c;
                     supply_centers = get_supplycenters bd c;
                     forces = get_forces bd c}
                in
                ai::acc)
            []
            lst
    in
    bd.players <- ai_lst @ bd.players

