open Bs_type
open Outside_types
open Js

let make_ident_name name =
  "_" ^ name

let make_aux_name name =
  "__" ^ name

let make_like_name name =
  "_" ^ name ^ "_like"

let make_aux parent name : Bs.type_def =
  let base = make_aux_name name in
  let typedef =
    if BatOption.is_some parent then
      to_ident ~variables:[to_variable "a"] base
    else
      to_ident base
  in
  (typedef, None)

let make_like parent_like name : Bs.type_def =
  let aux = make_aux_name name in
  let base = make_like_name name in
  let typedef = to_ident ~variables:[to_variable "a"] base in
  match parent_like with
  | None -> (typedef, None)
  | Some parent_like -> 
    let variables = [to_ident ~variables:[to_variable "a"] aux] in
    (typedef, Some (to_ident ~variables parent_like))

let make_ident name : Bs.type_def =
  let like = make_like_name name in
  let typedef = to_ident (make_ident_name name) in
  (typedef, Some (to_ident ~variables:[to_ident "_baseClass"] like))

let from_name parent name =
  let parent_like =
    match parent with
    | Some s -> Some(make_like_name s) 
    | None -> None
  in
  let aux_def = make_aux parent name in
  let like_def = make_like parent_like name in
  let def = make_ident name in
  [aux_def; like_def; def]

let from_obj (obj: js_obj) =
  from_name obj.inherits obj.name

let from_objs objs =
  List.map from_obj objs
  |> List.flatten