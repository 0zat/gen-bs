open Js
open Js_type
open Bs_let

let to_eval_line const : eval_line =
  let eval =
    match (const: Js.constant ) with
    | `Int i -> `Int i
    | `Float f -> `Float f 
    | `Bool true -> to_eval_ident ~modules:["Js"] "true_"
    | `Bool false -> to_eval_ident ~modules:["Js"] "false_"
    | `Null -> to_eval_ident ~modules:["Js"] "null"
  in
  [eval]

let to_let (name, const) =
  to_let_def ("_" ^ name) [] [] (to_eval_line const)