open Servo_dom
module Math = Js.Math

external window : _Window = "" [@@bs.val]
external document : _Document = "" [@@bs.val]
external console : _console = "" [@@bs.val]

let canvas: _HTMLCanvasElement = 
  match Document.getElementById "canvas" document with
  | None -> failwith "canvas is not found"
  | Some s -> (Element.downcast s)

let ctx = 
  match HTMLCanvasElement.getContext "2d" [||] canvas with
  | None -> failwith "canvas is not supported"
  | Some s -> (s: _CanvasRenderingContext2D)

let canvas_width = Window.innerWidth window 
let canvas_height = Window.innerHeight window 

let timer_tick = ref 0
let timer_total = ref 80

let rec loop _ =
  let callback = Js_unsafe.fn_mk1 loop in
  Window.requestAnimationFrame callback window;
  let open CanvasRenderingContext2D in
  setGlobalCompositeOperation ctx "destination-out";
  setFillStyle ctx "rgba(0, 0, 0, 0.5)";
  fillRect 0. 0. (float_of_int canvas_width) (float_of_int canvas_height) ctx;
  setGlobalCompositeOperation ctx "lighter";
  Firework.update ();
  Firework.draw ctx;
  Particle.update ();
  Particle.draw ctx;
  begin if !timer_tick >= !timer_total then begin
      let width = float_of_int canvas_width in
      let sx = Draw_objects.random (width /. 4.) (width /. 4. *. 3.) in
      let base_hue = Draw_objects.random 0. 360. in
      Firework.create 1 base_hue sx (float_of_int canvas_height);
      timer_total := Math.random_int 20 100;
      timer_tick := 0
    end
    else
      incr timer_tick
  end

let onload _ =
  let open HTMLCanvasElement in
  setWidth canvas canvas_width ;
  setHeight canvas  canvas_height ;
  loop 0. 

let () =
  Window.setOnload window (Js.Null.return (Js_unsafeJs_unsafe.fn_mk1 onload))
