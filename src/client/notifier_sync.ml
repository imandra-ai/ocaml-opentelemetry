include Util_thread.MCond
module IO = Generic_io.Direct_style

let delete = ignore

let trigger = signal

let register_bounded_queue = wakeup_from_bq
