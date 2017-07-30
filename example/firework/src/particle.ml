open Servo_dom  
open Draw_objects

module Conf : Draw_objects.Conf = struct

  let is_delete_elem elem = elem.alpha <= elem.decay

  let action_when_delete elem = ()

  let create_elem base_hue x y =
    Draw_objects.create_elem
      ~coord_count: 3
      ~x 
      ~y 
      ~speed: (random 1. 20.) 
      ~friction: 0.95 
      ~brightness: (random 50. 80.) 
      ~hue: (random (base_hue -. 50.) (base_hue +. 50.))
      ~gravity: (random 0.8 1.5)
      ~alpha: 1.
      ~decay: (random 0.005 0.05)
      ~angle_rad: (random 0. (Math._PI *. 2.))

end

include Draw_objects.Make(Conf)