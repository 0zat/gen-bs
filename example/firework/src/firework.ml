open Draw_objects

module Conf : Draw_objects.Conf = struct

  let is_delete_elem elem = 
    elem.speed <= 1.

  let action_when_delete elem = 
    let count = Math.random_int 25 50 in
    Particle.create count elem.hue elem.x elem.y

  let create_elem base_hue x y =
    Draw_objects.create_elem
      ~coord_count: 3
      ~x 
      ~y 
      ~speed: (random 15. 25.) 
      ~friction: 0.97 
      ~brightness: (random 50. 70.) 
      ~hue: base_hue
      ~gravity: 1.
      ~alpha: 1.
      ~decay: 0.
      ~angle_rad: (Math._PI /. 2. *. 3.)
end

include Draw_objects.Make(Conf)