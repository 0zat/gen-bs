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
  let ident = String.lowercase_ascii name in
  to_let_def ident [] [] (to_eval_line const)