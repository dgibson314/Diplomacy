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

let top_half = GPack.hbox ~height:500 ~packing:game_area#pack ()

let board = GPack.vbox ~width:720 ~packing:top_half#pack ()

let interactive_frame = GBin.frame ~label:"Diplomacy (in OCaml!)"
    ~label_xalign:0.5 ~border_width:5 ~height:500 ~width:300
    ~packing:top_half#pack ()

(* Top right *)
let controls = GPack.vbox ~packing:interactive_frame#add ()
let control_notebook = GPack.notebook ~tab_pos:`RIGHT ~homogeneous_tabs:true
    ~packing:controls#pack ()
let command_display = GText.view ~editable:false ~cursor_visible:false
    ~wrap_mode:`WORD ~show:true ~height:400
    ~packing:control_notebook#add ()
let command_input = GEdit.entry ~editable:true ~show:true
    ~packing:controls#pack ()

let button_area = GPack.hbox ~packing:controls#pack ()
let submit_button = GButton.button ~label:"Submit" 
    ~packing:button_area#pack ()
let adjudicate_button = GButton.button ~label:"Adjudicate"
    ~packing:button_area#pack ()
(*----------------------------------------------------*)


(*------------------------PIXBUFS--------------------*)
let board_pixbuf = GdkPixbuf.from_file "assets/map3.jpg"
let scaled_board_pixbuf = GdkPixbuf.create ~width:700 ~height:500
    ~bits:(GdkPixbuf.get_bits_per_sample board_pixbuf)
    ~has_alpha:(GdkPixbuf.get_has_alpha board_pixbuf) ()
let drawn_board_pixbuf = GdkPixbuf.copy scaled_board_pixbuf
let board_image = GMisc.image ~pixbuf:drawn_board_pixbuf
    ~width:700 ~height:500 ~packing:board#add ()
(*---------------------------------------------------*)


(*-----------------------HELPERS---------------------*)
(*TODO:
    * how does it work? *)
let print_to_cmd str =
    (*command_display#buffer#delete
        ~start:command_display#buffer#start_iter
        ~stop:command_display#buffer#end_iter;*)
    command_display#buffer#insert ~iter:command_display#buffer#end_iter str

let clear_cmd () =
    command_display#buffer#delete
        ~start:command_display#buffer#start_iter
        ~stop:command_display#buffer#end_iter

let waiting = ref (ref (Mutex.create ()))
let input_str = ref (ref "")
let readline waiting_ref string_ref =
    Mutex.lock (!waiting_ref);
    waiting := waiting_ref;
    input_str := string_ref


let main () =
    window#connect#destroy ~callback:Main.quit;
    factory#add_item "Quit" ~key:_Q ~callback:Main.quit;

    (* Create and scale board image *)
    GdkPixbuf.scale ~dest:scaled_board_pixbuf ~width:700 ~height:500 board_pixbuf;
    board_image#set_pixbuf scaled_board_pixbuf;

    command_input#connect#activate ~callback: (
        fun () -> print_to_cmd (command_input#text ^ "\n");
                  (!input_str) := command_input#text;
                  command_input#set_text "";
                  Mutex.unlock !(!waiting));

    window#show ();
    Main.main ()

