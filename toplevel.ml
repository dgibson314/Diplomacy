
let gui = Thread.create (Gui.main) ()
let game = Thread.create (Engine.run_game) ()

let _ = (Thread.join gui, Thread.join game)
