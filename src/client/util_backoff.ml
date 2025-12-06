type t = {
  mutable delay_s: float;
  min_delay_s: float;
  max_delay_s: float;
}

let create () = { delay_s = 0.001; min_delay_s = 0.001; max_delay_s = 20. }

let on_success self = self.delay_s <- max self.min_delay_s (self.delay_s /. 10.)

let on_error self = self.delay_s <- min self.max_delay_s (self.delay_s *. 2.)

let[@inline] cur_duration_s self = self.delay_s
