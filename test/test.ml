open Servo_dom

external window : window = "window" [@@bs.val]

let document = Window.document window

let elem = Document.getElementById document "sample"

let now = Performance.now (Window.performance window)

let () = 
  let alert str = Window.alert window str in
  alert "first";
  let i =
    let handler = `Callback (Js_unsafe.fn_mk1 (fun x -> alert x)) in
    Window.setTimeout 
      ~window 
      ~handler
      ~timeout:1000
      ~arguments:[|"called"|]
      ()
  in
  alert "end"
