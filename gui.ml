open GMain
open GdkKeysyms
open Board
open Board.String
open Game_utils


let locale = GtkMain.Main.init ()

let window = GWindow.window ~title:"Diplomacy" ~width:1280 ~height:830 ()

let main_container = GPack.vbox ~packing:window#add ()

(* Menu bar *)
let menubar = GMenu.menu_bar ~packing:main_container#pack ()
let factory = new GMenu.factory menubar
let accel_group = factory#accel_group
let file_menu = factory#add_submenu "Game"

(* Game menu *)
let factory = new GMenu.factory file_menu ~accel_group
let factory = new GMenu.factory file_menu ~accel_group




(*-------------------VARIOUS PACKING SHIT-------------*)
let game_area = GPack.vbox ~packing:main_container#add ()

(* contains board and options for press and shit like that *)
let command_area = GPack.hbox ~height:500 ~packing:game_area#pack ()

let board = GPack.vbox ~width:720 ~packing:command_area#pack ()

let interactive_area = GBin.frame ~label:"Diplomacy (in OCaml!)"
                            ~label_xalign:0.5 
                            ~border_width:5 ~height:500 ~width:300
                            ~packing:command_area#pack ()
let controls = GPack.vbox ~packing:interactive_area#add ()
let scrolling_text = GBin.scrolled_window ~vpolicy:`AUTOMATIC ~height:400
                            ~packing:controls#add () 

let info_area = GPack.table ~height:250 ~columns:5 ~packing:game_area#pack () 
let notebook = GPack.notebook ~tab_pos:`TOP ~homogeneous_tabs:true
    ~packing:(info_area#attach ~left:0 ~right:5 ~top:0) ()

let game_info_frame = GBin.frame ~label:"Game Info" ~width:1200 ~height:230
    ~packing:notebook#add ()
let game_info_scroll = GBin.scrolled_window ~height:220
    ~packing:game_info_frame#add ()
let game_info_display = GText.view ~editable:false ~cursor_visible:false
                                   ~wrap_mode:`WORD
                                   ~show:true
                                   ~packing:game_info_scroll#add ()
let supply_centers = GBin.frame ~label:"Supply Centers" ~width:1200 
    ~height:230 ~packing:notebook#add ()
let supply_centers_hbox = GPack.hbox ~width:1200 
    ~packing:supply_centers#add ()
let england_centers_scroll = GBin.scrolled_window ~width:170
    ~packing:supply_centers_hbox#add ()
let england_centers = GText.view ~editable:false ~cursor_visible:false
    ~wrap_mode:`WORD ~show:true ~packing:england_centers_scroll#add ()
let france_centers = GText.view ~editable:false ~cursor_visible:false
    ~wrap_mode:`WORD ~show:true ~packing:supply_centers_hbox#add ()
let germany_centers = GText.view ~editable:false ~cursor_visible:false
    ~wrap_mode:`WORD ~show:true ~packing:supply_centers_hbox#add ()
let russia_centers = GText.view ~editable:false ~cursor_visible:false
    ~wrap_mode:`WORD ~show:true ~packing:supply_centers_hbox#add ()



(*-----------------------PIXBUFS---------------------------*)
(* load up the game board *)
let board_pixbuf = GdkPixbuf.from_file "assets/map3.jpg"
let scaled_board_pixbuf = GdkPixbuf.create ~width:700
                                            ~height:500
                    ~bits:(GdkPixbuf.get_bits_per_sample board_pixbuf)
                    ~has_alpha:(GdkPixbuf.get_has_alpha board_pixbuf) ()
let drawn_board_pixbuf = GdkPixbuf.copy scaled_board_pixbuf
let board_image = GMisc.image ~pixbuf:drawn_board_pixbuf
                              ~width:700
                              ~height:500
                              ~packing:board#add ()


(* load up the army box avatar *)
let box_pixbuf = GdkPixbuf.from_file "assets/box.png"
let scaled_box_pixbuf = GdkPixbuf.create ~width:10 ~height:10
                    ~bits:(GdkPixbuf.get_bits_per_sample box_pixbuf)
                    ~has_alpha:(GdkPixbuf.get_has_alpha box_pixbuf) ()
(*---------------------------------------------------------*)


let command_display = GText.view ~editable:false ~cursor_visible:false
                                 ~wrap_mode:`WORD
                                 ~show:true
                                 ~packing:scrolling_text#add ()

let command_input = GEdit.entry ~editable:true ~show:true 
                               ~packing:controls#pack ()

let button_area = GPack.hbox ~packing:controls#pack ()

let submit_button = GButton.button ~label:"Submit Orders"
                                    ~packing:button_area#pack ()

let cheat_button = GButton.button ~label:"Cheat"
                                    ~packing:button_area#pack ()

                                    
(* TODO:
    * HTF does this work *)
let print_to_cmd str =
    command_display#buffer#insert ~iter:command_display#buffer#end_iter str;
    scrolling_text#vadjustment#set_value
        (scrolling_text#vadjustment#upper -. 
         scrolling_text#vadjustment#page_size +. 500.)

let print_to_display (display : GText.view) str =
    display#buffer#insert ~iter:display#buffer#end_iter str

(* apparently some helper variables and functions *)
let waiting = ref (ref (Mutex.create ()))
let input_str = ref (ref "")
let readline waiting_ref string_ref =
    Mutex.lock (!waiting_ref);
    waiting := waiting_ref;
    input_str := string_ref






let main () =
    window#connect#destroy ~callback:Main.quit;

    factory#add_item "Quit" ~key:_Q ~callback:Main.quit;

    (* create and scale board image *)
    GdkPixbuf.scale ~dest:scaled_board_pixbuf ~width:700 
                    ~height:500 board_pixbuf;
    (* draw the board *)
    board_image#set_pixbuf scaled_board_pixbuf;

    (* command input and display *)
    command_input#connect#activate ~callback: (
        fun () -> print_to_cmd (command_input#text ^ "\n");
                  (!input_str) := command_input#text;
                  command_input#set_text "";
                  Mutex.unlock !(!waiting));

    submit_button#connect#pressed ~callback: (
        fun () -> print_to_cmd (command_input#text ^ "\n");
                  (!input_str) := command_input#text;
                  command_input#set_text "";
                  Mutex.unlock !(!waiting));


    window#show ();
    Main.main ()

