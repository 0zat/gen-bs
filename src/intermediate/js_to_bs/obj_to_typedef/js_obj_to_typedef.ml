open Bs_type
open Outside_types
open Js

let make_ident_name name =
  to_ident name |> print

let make_aux parent ident_name : Bs.type_def =
  let base = "_" ^ ident_name in
  let typedef =
    if BatOption.is_some parent then
      to_ident ~variables:[to_variable "a"] base
    else
      to_ident base
  in
  (typedef, None)

let make_like parent_like ident_name : Bs.type_def =
  let aux = "_" ^ ident_name in
  let base = ident_name ^ "_like" in
  let typedef = to_ident ~variables:[to_variable "a"] base in
  match Outside_types.find base with
  | Some(modules, def) -> 
  (typedef, Some (to_ident ~modules ~variables:[to_variable "a"] def))
  | None ->
    match parent_like with
    | None -> (typedef, None)
    | Some parent_like -> 
      let variables = [to_ident ~variables:[to_variable "a"] aux] in
      (typedef, Some (to_ident ~variables parent_like))

let make_ident ident_name : Bs.type_def =
  let like = ident_name ^ "_like" in
  let typedef = to_ident ident_name in
  match Outside_types.find ident_name with
  | Some(modules, def) -> (typedef, Some (to_ident ~modules def))
  | None -> 
    (typedef, Some (to_ident ~variables:[to_ident "_baseClass"] like))

let from_name parent name =
  let ident_name = make_ident_name name in
  let parent_like =
    match parent with
    | Some s -> Some(s ^ "_like")
    | None -> None
  in
  let aux_def = make_aux parent ident_name in
  let like_def = make_like parent_like ident_name in
  let def = make_ident ident_name in
  [aux_def; like_def; def]

let from_obj (obj: js_obj) =
  from_name obj.inherits obj.name

let from_objs objs =
  List.map from_obj objs
  |> List.flatten