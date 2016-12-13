open GMain
open GdkKeysyms

let locale = GtkMain.Main.init ()

let rotate_book notebook () =
    notebook#set_tab_pos
        (match notebook#tab_pos with
         | `BOTTOM -> `LEFT
         | `LEFT -> `TOP
         | `TOP -> `RIGHT
         | `RIGHT -> `BOTTOM)

let tabsborder_book notebook () =
    notebook#set_show_tabs (not notebook#show_tabs);
    notebook#set_show_border (not notebook#show_border)

let remove_book notebook () =
    notebook#remove_page notebook#current_page;
    ()

let main () =
    let window = GWindow.window ~title:"Notebook" ~border_width:10 () in
    window#connect#destroy ~callback:GMain.Main.quit;

    let table = GPack.table ~rows:3 ~columns:6 ~packing:window#add () in

    let notebook = GPack.notebook ~tab_pos:`TOP
        ~packing:(table#attach ~left:0 ~right:6 ~top:0) () in

    for i = 1 to 5 do
        let text = "Append Frame " ^ string_of_int i in
        let label = GMisc.label ~text:("Page " ^ string_of_int i) () in
        let frame = GBin.frame ~label:text ~width:100 ~height:75 
                ~border_width:10
                ~packing:notebook#add () in
        let label = GMisc.label ~text ~packing:frame#add () in
        ()
    done;

    let label = GMisc.label ~text:"Add page" () in
    let checkbutton = GButton.check_button ~label:"Check me"
            ~packing:notebook#add ()
    in
    checkbutton#misc#set_size_request ~width:100 ~height:75 ();

    for i = 1 to 5 do
        let text = "Prepend Frame " ^ string_of_int i in
        let label = GMisc.label ~text:("PPage " ^ string_of_int i) () in
        let frame = GBin.frame ~label:text ~width:100 ~height:75
            ~border_width:10
            ~packing:notebook#add () in
        let label = GMisc.label ~text ~packing:frame#add () in
        ()
    done;

    notebook#goto_page 3;

    let button = GButton.button ~label:"close"
        ~packing:(table#attach ~left:0 ~top:1) () in
    button#connect#clicked ~callback:Main.quit;

    let button = GButton.button ~label:"next page"
        ~packing:(table#attach ~left:1 ~top:1) () in
    button#connect#clicked ~callback:notebook#next_page;

    let button = GButton.button ~label:"prev page"
        ~packing:(table#attach ~left:2 ~top:1) () in
    button#connect#clicked ~callback:notebook#previous_page;

    let button = GButton.button ~label:"tabl position"
        ~packing:(table#attach ~left:3 ~top:1) () in
    button#connect#clicked ~callback:(rotate_book notebook);

    let button = GButton.button ~label:"tabs/border on/off"
        ~packing:(table#attach ~left:4 ~top:1) () in
    button#connect#clicked ~callback:(tabsborder_book notebook);

    let button = GButton.button ~label:"remove page"
        ~packing:(table#attach ~left:5 ~top:1) () in
    button#connect#clicked ~callback:(remove_book notebook);

    window#show ();
    Main.main ()

let _ = Printexc.print main ()
