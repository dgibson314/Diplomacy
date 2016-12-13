open GMain
open GdkKeysyms
open Board
open Board.String

let locale = GtkMain.Main.init ()

let window = GWindow.window ~title:"Start" ~width:200 ~height:280 ()

let main_container = GPack.vbox ~packing:window#add ()

(* Menu bar *)
let menubar = GMenu.menu_bar ~packing:main_container#pack ()
let factory = new GMenu.factory menubar
let accel_group = factory#accel_group
let file_menu = factory#add_submenu "Game"

(* Game menu *)
let factory = new GMenu.factory file_menu ~accel_group
let factory = new GMenu.factory file_menu ~accel_group

let labelbox = GPack.vbox  ~packing:main_container#add ()
let buttonbox = GPack.vbox ~height:300 ~packing:main_container#add ()

(* label *)
let label1 = GMisc.label ~text:"Select a country" 
            ~packing:(labelbox#pack ~padding:20 ~expand:true ~fill:false) ()

(*  radio buttons *)
let hello lbl =
    print_endline lbl;
    flush stdout


let england = GButton.radio_button ~label:"England" 
                        ~packing:buttonbox#pack ()
let france = GButton.radio_button ~group:england#group ~label:"France"
                        ~packing:buttonbox#pack ()
let germany = GButton.radio_button ~group:england#group ~label:"Germany"
                        ~packing:buttonbox#pack ()
let russia = GButton.radio_button ~group:england#group ~label:"Russia"
                        ~packing:buttonbox#pack ()
let turkey = GButton.radio_button ~group:england#group ~label:"Turkey"
                        ~packing:buttonbox#pack ()
let austria = GButton.radio_button ~group:england#group ~label:"Austria"
                        ~packing:buttonbox#pack ()
let italy = GButton.radio_button ~group:england#group ~label:"Italy"
                        ~packing:buttonbox#pack ()
let random = GButton.radio_button ~group:england#group ~label:"Random"
                        ~active:true ~packing:buttonbox#pack ()

let submit = GButton.button ~label:"Submit" ~packing:buttonbox#pack ()


let main () =
    window#connect#destroy ~callback:Main.quit;
    factory#add_item "Quit" ~key:_Q ~callback:Main.quit;

    england#connect#pressed ~callback:(fun () -> hello england#label);
    france#connect#pressed ~callback:(fun () -> hello france#label);

    window#show ();
    Main.main ()

let _ = main ()

