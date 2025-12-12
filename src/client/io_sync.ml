include Generic_io.Direct_style

let sleep_s = Thread.delay

let[@inline] spawn f = ignore (Util_thread.start_bg_thread f : Thread.t)
