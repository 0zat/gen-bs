open Servo_dom
module Math = Js.Math

type elem = {
  mutable x: float;
  mutable y: float;
  coordinates: (float * float) Queue.t;
  mutable speed: float;
  friction: float;
  brightness: float;
  hue: float;
  gravity: float;
  mutable alpha: float;
  decay: float;
  angle_rad: float;
}

let random min max = (Math.random ()) *. (max -. min) +. min

let create_elem ~coord_count ~x ~y ~speed ~friction ~brightness ~hue ~gravity ~alpha ~decay ~angle_rad =
    let coordinates = Queue.create () in
    Array.make coord_count (x, y)
    |> Array.iter (fun x -> Queue.add x coordinates)
    ;
    {
      x ;
      y ;
      coordinates;
      speed ; 
      friction ; 
      brightness ; 
      hue ;
      gravity ;
      alpha ;
      decay ;
      angle_rad 
    }

module type Conf = sig
  val is_delete_elem: elem -> bool
  val action_when_delete: elem -> unit
  val create_elem: float -> float -> float -> elem
end

module Make (Conf: Conf) = struct

  let elems = ref []

  let update_elem elem =
    Queue.pop elem.coordinates;
    Queue.add (elem.x, elem.y) elem.coordinates;
    elem.speed <- elem.speed *. elem.friction;
    elem.x <- elem.x +. (Math.cos elem.angle_rad) *. elem.speed;
    elem.y <- elem.y +. (Math.sin elem.angle_rad) *. elem.speed +. elem.gravity;
    elem.alpha <- elem.alpha -. elem.decay;
    if Conf.is_delete_elem elem then
      (Conf.action_when_delete elem; false)
    else
      true

  let draw_elem ctx elem =
    let open CanvasRenderingContext2D in
    beginPath ctx;
    let x, y = Queue.peek elem.coordinates in 
    moveTo ~x ~y ctx;
    lineTo elem.x elem.y ctx;
    let make_stroke_style elem = 
      Printf.sprintf "hsla(%f, 100%%, %f%%,%f)" elem.hue elem.brightness elem.alpha
    in
    setStrokeStyle ctx (make_stroke_style elem);
    stroke ctx

  let update () =
    elems := List.filter (fun elem -> update_elem elem) !elems

  let draw ctx =
    List.iter (draw_elem ctx) !elems

  let create count base_hue x y =
    for _ = 1 to count do
      let elem = Conf.create_elem base_hue x y in
      elems := elem :: !elems
    done
end