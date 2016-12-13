open GMain
open GdkKeysyms

let locale = GtkMain.Main.init ()

let create_bbox direction title spacing child_width child_height layout =
    let frame = GBin.frame ~label:title () in
    let bbox = GPack.button_box direction ~border_width:5 ~layout
        ~child_height ~child_width ~spacing ~packing:frame#add () in
    GButton.button ~stock:`OK ~packing:bbox#add ();
    GButton.button ~stock:`CANCEL ~packing:bbox#add ();
    GButton.button ~stock:`HELP ~packing:bbox#add ();
    frame#coerce

let main () =
    let window = GWindow.window ~title:"Button boxes" ~border_width:10 () in
    window#connect#destroy ~callback:Main.quit;

    let main_vbox = GPack.vbox ~packing:window#add () in

    let frame_horz = GBin.frame ~label:"Horizontal Button boxes"
        ~packing:(main_vbox#pack ~expand:true ~fill:true ~padding:10) () in

    let vbox = GPack.vbox ~border_width:10 ~packing:frame_horz#add () in

    vbox#add (create_bbox `HORIZONTAL "Spread (spacing 40)" 40 85 20 `SPREAD);
    vbox#pack (create_bbox `HORIZONTAL "Edge (spacing 30)" 30 85 20 `EDGE)
        ~expand:true ~fill:true ~padding:5;
    vbox#pack (create_bbox `HORIZONTAL "Start (spacing 20)" 20 85 20 `START)
        ~expand:true ~fill:true ~padding:5;
    vbox#pack (create_bbox `HORIZONTAL "End (spacing 10)" 10 85 20 `END)
        ~expand:true ~fill:true ~padding:5;

    window#show ();

    Main.main ()

let _ = Printexc.print main () 


