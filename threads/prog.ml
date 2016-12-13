open Mutex

let m = Mutex.create ()

let printn n =
    for i=1 to 4 do
        Mutex.lock m;
        print_int n; print_newline();
        Mutex.unlock m
    done

let t1 = Thread.create printn 1
let t2 = Thread.create printn 2
let t3 = Thread.create printn 3

let _ = (Thread.join t1, Thread.join t2, Thread.join t3)
